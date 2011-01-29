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

#include "pool.h"
#include "types.h"

void gc_init(void);

long baker_collect();
object *push_root(object **stack);
void pop_root(object **stack);

object *alloc_object(char needs_finalization);
long get_alloc_count();

void *xmalloc(size_t size); /* exit() on failure */
void *MALLOC(size_t size);  /* mmap()ed */
void *REALLOC(void *p, size_t new);  /* mmap()ed */
void FREE(void *p);       /* mmap()ed */

int save_image(char *filename);
int load_image(char *filename);

typedef struct doubly_linked_list {
  object *head;
  object *tail;
  long num_objects;
} doubly_linked_list;

typedef struct stack_set {
  void **objs;
  long top;
  long size;
} stack_set;

typedef struct global_state {
  /* GC */
  pool_t *global_pool;

  doubly_linked_list Active_Heap_Objects;
  doubly_linked_list Old_Heap_Objects;

  object *Next_Free_Object;
  struct stack_set *Root_Objects;
  struct stack_set *Finalizable_Objects;
  struct stack_set *Finalizable_Objects_Next;

  long Alloc_Count;
  long Next_Heap_Extension;

  char current_color;

  /* VM */
  object *cc_bytecode;
  object *error_sym;

  /* interp */
  char debug_enabled;
  long stack_top;

  /* symbols */
  object *empty_list;
  object *empty_vector;
  object *false;
  object *true;
  object *symbol_table;
  object *quote_symbol;
  object *quasiquote_symbol;
  object *unquote_symbol;
  object *unquotesplicing_symbol;
  object *set_symbol;
  object *if_symbol;
  object *begin_symbol;
  object *lambda_symbol;
  object *macro_symbol;
  object *stdin_symbol;
  object *stdout_symbol;
  object *stderr_symbol;
  object *eof_object;
  object *exit_hook_symbol;

  object *empty_env;
  object *env;
  object *vm_env;

} global_state;

extern global_state *g;

#endif
