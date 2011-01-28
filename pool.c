/* pool.c - Allocation Pool Library
 * Copyright (C) 2007 Christopher Wellons <mosquitopsu@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <bits/mman.h>
#include "pool.h"
#include "gc.h"

size_t default_pool_size = 1048576;
int miss_limit = 8;
int pool_scale = 2;

void* new_mmap(size_t size) {
  void *p = mmap(NULL, size, PROT_READ | PROT_WRITE,
		 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  printf("mmap(): %p (%ld kB)\n", p, size / 1024);
  fflush(stdout);
  return p;
}

/* Used internally to allocate more pool space. */
static subpool_t *create_subpool_node (size_t size);

pool_t *create_pool (size_t init_size)
{
  /* if init_size == 0, user didn't want to choose one */
  if (init_size == 0)
    {
      init_size = default_pool_size;
    }

  pool_t *new_pool = (pool_t *) xmalloc (sizeof (pool_t));

  /* allocate first subpool */
  new_pool->pools = create_subpool_node (init_size);
  new_pool->first = new_pool->pools;

  if (new_pool->first == NULL)
    return NULL;

  return new_pool;
}

/* Returns a pointer to the allocated size bytes from the given
 * pool. */
void *pool_alloc (pool_t * source_pool, size_t size)
{
  subpool_t *cur, *last;
  void *chunk = NULL;
  size_t s = sizeof(size_t);

  cur = source_pool->first;
  if (cur->misses > miss_limit)
    {
      /* this pool doesn't seem to be any good anymore */
      source_pool->first = source_pool->first->next;
    }

  do
    {
      /* Check this pool's free list. */
      freed_t *lastf = NULL, *f = cur->freed;
      while (f != NULL)
	{
	  if (f->size >= size)
	    {
	      cur->misses = 0;
	      if (lastf == NULL)
		cur->freed = f->next;
	      else
		lastf->next = f->next;
	      void *chunk = f->p;
	      free (f);
	      return chunk;
	    }
	  lastf = f;
	  f = f->next;
	}

      if ((size + s) <= (size_t) (cur->free_end - cur->free_start))
	{
	  /* cut off a chunk and return it */
	  chunk = cur->free_start;
	  cur->free_start += size + s;
	  cur->misses = 0;
	}
      else
	{
	  /* current pool is too small */
	  cur->misses++;
	}

      last = cur;
      cur = cur->next;
    }
  while (cur != NULL && chunk == NULL);

  /* No existing pools had enough room. Make a new one. */
  if (chunk == NULL)
    {
      /* double the size of the last one */
      size_t new_size = last->size * pool_scale;
      if (new_size <= (size + s))
	{
	  /* square requested size if its much bigger */
	  new_size = (size + s) * pool_scale * pool_scale;
	}

      /* create new subpool */
      last->next = create_subpool_node (new_size);
      cur = last->next;

      if (cur == NULL)		/* failed to allocate subpool */
	return NULL;

      /* chop off requested amount */
      chunk = cur->free_start;
      cur->free_start += size + s;
    }
  ((size_t *) chunk)[0] = size;
  return chunk + s;
}

/* Reallocate pool memory at location. */
void *pool_realloc (pool_t * source_pool, void * p, size_t new)
{
  size_t old = *(((size_t *) p) - 1);
  if (old >= new)
    return p;
  void *np = pool_alloc (source_pool, new);
  memcpy(np, p, old);
  pool_free (source_pool, p);
  return np;
}

/* Return memory to the pool. */
void pool_free (pool_t * source_pool, void *p)
{
  subpool_t *cur, *next;

  cur = source_pool->first;
  next = cur->next;
  while (next != NULL)
    {
      if (p >= ((void *) cur) && p < ((void *) next))
	break;
      cur = next;
      next = cur->next;
    }
  freed_t *f = (freed_t *) xmalloc (sizeof(freed_t));
  f->p = p;
  f->size = *(((size_t *) p) - 1);
  f->next = cur->freed;
  cur->freed = f;
}

subpool_t *create_subpool_node (size_t size)
{
  subpool_t *new_subpool = (subpool_t *) xmalloc (sizeof (subpool_t));

  /* allocate subpool memory */
  new_subpool->mem_block = new_mmap (size);
  if (new_subpool->mem_block == NULL)
    {
      free (new_subpool);
      return NULL;
    }

  /* initialize data */
  new_subpool->free_start = new_subpool->mem_block;
  new_subpool->free_end = new_subpool->mem_block + size;
  new_subpool->size = size;
  new_subpool->misses = 0;
  new_subpool->freed = NULL;
  new_subpool->next = NULL;

  return new_subpool;
}

/* Dump entire pool to file that can be read back in later to the same
 * place in memory. */
int pool_dump (pool_t * pool, char *file)
{
  FILE *f = fopen(file, "w");
  if (f == NULL) return -1;
  subpool_t *cur = pool->pools;
  while (cur != NULL)
    {
      fwrite(&(cur->size), sizeof(size_t), 1, f);
      fwrite(&(cur->mem_block), sizeof(void *), 1, f);
      fwrite(cur->mem_block, cur->size, 1, f);
      cur = cur->next;
    }
  fclose(f);
  return 0;
}

/* Read the pool from the given file into memory. */
int pool_load (char * file)
{
  int fd = open(file, O_RDWR);
  off_t loc = 0;
  while (1) {
    void *address;
    size_t size, r;
    r = read(fd, &size, sizeof(size_t));
    if (r == 0) break;
    r = read(fd, &address, sizeof(void *));
    if (r == 0) break;
    loc += sizeof(void *);
    loc += sizeof(size_t);
    mmap(address, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, loc);
    loc += size;
    lseek(fd, size, SEEK_CUR);
  }
  return 0;
}
