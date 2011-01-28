/* pool.h - Allocation Pool Library
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

#ifndef POOL_H
#define POOL_H

extern size_t default_pool_size;

typedef struct freed_t
{
  void *p;			/* location of free memory. */
  size_t size;			/* size of free memory */
  struct freed_t *next;		/* next chunk of free memory */
} freed_t;

typedef struct subpool_t
{
  void *mem_block;		/* beginning of the memory block */
  void *free_start;		/* start of free segment */
  void *free_end;		/* end of free segment */
  size_t size;			/* total size of the block */
  int misses;			/* allocation misses for this subpool */
  freed_t *freed;		/* freed memory */
  struct subpool_t *next;	/* next subpool in this pool */
} subpool_t;

typedef struct pool_t
{
  subpool_t *pools;		/* first element in linked list */
  subpool_t *first;		/* first good subpool in list */
} pool_t;

/* Create a pool with a given initial size. If init_size is 0, the
 * default size is used. Returns NULL if malloc() fails. */
pool_t *create_pool (size_t init_size);

/* Returns a pointer to the allocated size bytes from the given
 * pool. Returns NULL if malloc() fails. */
void *pool_alloc (pool_t * source_pool, size_t size);

/* Reallocate pool memory at location. */
void *pool_realloc (pool_t * source_pool, void * p, size_t new);

/* Free memory back into the pool. */
void pool_free (pool_t * pool, void *p);

/* Dump entire pool to file that can be read back in later to the same
 * place in memory. */
int pool_dump (pool_t * pool, char *file);

/* Read the pool from the given file into memory. */
int pool_load (char * file);

#endif
