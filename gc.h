/**
 * Copyright 2010 Brian Taylor
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

#ifndef GC_H
#define GC_H

#include "types.h"

void gc_init(void);

long baker_collect();
object *push_root(object **stack);
void pop_root(object **stack);

object *alloc_object(char needs_finalization);
long get_alloc_count();

void *xmalloc(size_t size); /* exit() on failure */
void *MALLOC(size_t size);  /* mmap()ed */
void *REALLOC(void *p, size_t old, size_t new);  /* mmap()ed */
void FREE(void *p);       /* mmap()ed */

#endif
