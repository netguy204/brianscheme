/* pool.c - Allocation Pool Library
 *
 * Copyright (C) 2007, 2011 Christopher Wellons <mosquitopsu@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdint.h>
#include <sys/mman.h>
#include <zlib.h>
#include "pool.h"
#include "gc.h"
#include "tlsf.h"

#define ZCHUNK 16384

static const size_t hdr = sizeof(void *) + sizeof(size_t);

size_t default_pool_size = 1048576;
int miss_limit = 8;
int pool_scale = 2;
size_t init_freed_stack = 256;

void *new_mmap(size_t size) {
  void *p = mmap(NULL, size, PROT_READ | PROT_WRITE,
		 MAP_PRIVATE | MAP_ANON, -1, 0);
  if(p == MAP_FAILED) {
    fprintf(stderr, "error: mmap() failed to create mempool: %s\n",
	    strerror(errno));
    exit(1);
  }
  return p;
}

/* Used internally to allocate more pool space. */
static subpool_t *create_subpool_node(size_t size);

pool_t *create_pool(size_t min_alloc, size_t init_alloc, void **init) {
  /* Make sure it aligns as a page. */
  size_t ps = sysconf(_SC_PAGE_SIZE);
  size_t init_size = (default_pool_size / ps) * ps;

  /* allocate first subpool and use it for the pool */
  subpool_t *first = create_subpool_node(init_size);
  pool_t *new_pool = first->free_start;
  first->free_start += sizeof(pool_t);
  new_pool->pools = first;
  new_pool->first = new_pool->pools;
  new_pool->min_alloc = min_alloc;

  if(init_alloc > 0 && init != NULL) {
    *init = first->free_start;
    first->free_start += init_alloc;
  }

  init_memory_pool(first->free_end - first->free_start - 8, first->free_start);

  return new_pool;
}

/* Returns a pointer to the allocated size bytes from the given
 * pool. */
void *pool_alloc(pool_t * source_pool, size_t size) {
  /* next 8 byte aligned size */
  size = (size + 8 - 1) & ~(8 - 1);

  void * storage = malloc_ex(size, source_pool->pools->free_start);
  if(storage != NULL) {
    return storage;
  } else {
    subpool_t *last = source_pool->first;

    /* double the size of the last one */
    size_t new_size = last->size * pool_scale;

    /* create new subpool */
    last->next = create_subpool_node(new_size);
    last = last->next;
    source_pool->first = last;

    /* add it to the managed set */
    add_new_area(last->free_start, last->free_end - last->free_start - 8, source_pool->pools->free_start);

    /* call ourselves recursively to try again to satisfy the request */
    return pool_alloc(source_pool, size);
  }
}

/* Reallocate pool memory at location. */
void *pool_realloc(pool_t * source_pool, void *p, size_t new) {
  size_t old = *(((size_t *) p) - 1);
  if(old >= new)
    return p;
  void *np = pool_alloc(source_pool, new);
  memcpy(np, p, old);
  pool_free(source_pool, p);
  return np;
}

/* Return memory to the pool. */
void pool_free(pool_t * source_pool, void *p) {
  free_ex(p, source_pool->pools->free_start);
}

subpool_t *create_subpool_node(size_t size) {
  /* allocate subpool memory */
  void *block = new_mmap(size);
  subpool_t *new_subpool = block + hdr;
  new_subpool->mem_block = block;

  /* initialize data */
  ((size_t *) new_subpool->mem_block)[0] = size;
  ((void **)(sizeof(size_t *) + new_subpool->mem_block))[0]
    = new_subpool->mem_block;
  new_subpool->free_start = new_subpool->mem_block + hdr + sizeof(subpool_t);
  new_subpool->free_end = new_subpool->mem_block + size;
  new_subpool->size = size;
  new_subpool->misses = 0;
  new_subpool->next = NULL;

  /* freed stack (create later) */
  new_subpool->freedb = NULL;
  new_subpool->freedp = NULL;
  new_subpool->freed_size = 0;

  return new_subpool;
}

/* Dump entire pool to file that can be read back in later to the same
 * place in memory. */
int pool_dump(pool_t * pool, char *file, int compress) {
  int fd = open(file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
  if(fd < 0)
    return -1;
  subpool_t *cur = pool->pools;

  gzFile gz;
  if(compress != 0) {
    gz = gzdopen(fd, "w");
    gzsetparams(gz, compress, Z_DEFAULT_STRATEGY);
  }

  while(cur != NULL) {
    if(compress == 0) {
      write(fd, cur->mem_block, cur->size);
    }
    else {
      gzwrite(gz, cur->mem_block, cur->size);
    }
    cur = cur->next;
  }

  if(compress != 0)
    gzclose(gz);
  else
    close(fd);
  return 0;
}

void *pool_loadz(char *file, off_t offset);

/* Read the pool from the given file into memory. */
void *pool_load(char *file, off_t offset) {
  /* Check for a compressed image. */
  FILE *zcheck = fopen(file, "r");
  if(zcheck == NULL) {
    fprintf(stderr, "error: failed to open %s: %s\n", file, strerror(errno));
    return NULL;
  }
  if(offset > 0)
    fseek(zcheck, offset, SEEK_SET);
  uint16_t zheader;
  fread(&zheader, 1, 2, zcheck);
  if(zheader == 0x8b1f) {
    /* This is a compressed stream. */
    fclose(zcheck);
    return pool_loadz(file, offset);
  }
  fclose(zcheck);

  int fd = open(file, O_RDONLY);
  if(fd < 0) {
    fprintf(stderr, "error: failed to open %s: %s\n", file, strerror(errno));
    return NULL;
  }
  off_t loc = offset;
  if(offset > 0)
    lseek(fd, offset, SEEK_SET);
  void *first = NULL;
  while(1) {
    void *address;
    size_t size, r;
    r = read(fd, &size, sizeof(size_t));
    if(r == 0)
      break;
    r = read(fd, &address, sizeof(void *));
    if(r == 0)
      break;
    if(first == NULL)
      first = address;
    void *p = mmap(address, size, PROT_READ | PROT_WRITE,
		   MAP_PRIVATE | MAP_FIXED, fd, loc);
    if(p == MAP_FAILED) {
      fprintf(stderr, "error: failed to mmap() %s: %s\n",
	      file, strerror(errno));
      return NULL;
    }
    loc += size;
    lseek(fd, size - hdr, SEEK_CUR);
  }
  if(first == NULL) {
    fprintf(stderr, "error: empty image file: %s\n", file);
    return NULL;
  }
  return first + hdr + sizeof(subpool_t) + sizeof(pool_t);
}

/* Load a compressed pool. Rather than mmap() a file, we just mmap()
 * MAP_ANON again and write into it. */
void *pool_loadz(char *file, off_t offset) {
  int fd = open(file, O_RDONLY);
  if(fd < 0) {
    fprintf(stderr, "error: failed to open %s: %s\n", file, strerror(errno));
    return NULL;
  }
  if(offset > 0)
    lseek(fd, offset, SEEK_SET);
  gzFile gz = gzdopen(fd, "r");
  void *first = NULL;
  while(1) {
    void *address;
    size_t size, r;
    r = gzread(gz, &size, sizeof(size_t));
    if(r == 0)
      break;
    r = gzread(gz, &address, sizeof(void *));
    if(r == 0)
      break;

    if(first == NULL)
      first = address;
    void *p = mmap(address, size, PROT_READ | PROT_WRITE,
		   MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
    if(p == MAP_FAILED) {
      fprintf(stderr, "error: loadz failed to mmap() %s: %s\n",
	      file, strerror(errno));
      return NULL;
    }
    memcpy(address, &size, sizeof(size));
    memcpy(address + sizeof(size), &address, sizeof(address));
    size_t amt = size - hdr;
    r = gzread(gz, address + hdr, amt);
    if(r != amt) {
      fprintf(stderr, "error: failed to decompress image %s\n", file);
      return NULL;
    }
  }
  if(first == NULL) {
    fprintf(stderr, "error: empty image file: %s\n", file);
    return NULL;
  }
  gzclose(gz);
  return first + hdr + sizeof(subpool_t) + sizeof(pool_t);
}
