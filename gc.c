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

#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "symbols.h"
#include "hashtab.h"

/* enable gc debuging by defining
 * DEBUG_GC
 */

void *MALLOC(long size) {
  void *obj = malloc(size);
  if(obj == NULL) {
    fprintf(stderr, "out of memory\n");
    exit(1);
  }
  return obj;
}

void throw_gc_va(char *msg, va_list args) {
  vfprintf(stderr, msg, args);
  exit(2);
}

void throw_gc(char *msg, ...) {
  va_list args;
  va_start(args, msg);
  throw_gc_va(msg, args);
  va_end(args);
}

#ifdef DEBUG_GC
#include <execinfo.h>

void print_backtrace() {
#define MAX_FRAMES 30
  void *buffer[MAX_FRAMES];
  int frames = backtrace(buffer, MAX_FRAMES);
  backtrace_symbols_fd(buffer, frames, 2);
}

void debug_gc(char *msg, ...) {
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
}

void assert_gc(char test, char *msg, ...) {
  va_list args;
  va_start(args, msg);

  if(!test) {
    throw_gc_va(msg, args);
  }

  va_end(args);
}
#else
#define assert_gc(a, b, ...)
#define print_backtrace()
#define debug_gc(msg, ...)
#endif


typedef struct doubly_linked_list {
  object * head;
  object * tail;
  long num_objects;
} doubly_linked_list;

void move_object_to_head(object* obj, doubly_linked_list* src,
			 doubly_linked_list* dest) {
  /* unlink from the old list */
  if(obj->prev == NULL) {
    src->head = obj->next;
  } else {
    obj->prev->next = obj->next;
  }
  if(obj->next == NULL) {
    src->tail = obj->prev;
  } else {
    obj->next->prev = obj->prev;
  }
  src->num_objects--;

  /* link into new list */
  if(dest->head == NULL) {
    dest->head = obj;
    dest->tail = obj;
    obj->next = NULL;
    obj->prev = NULL;
  } else {
    obj->prev = NULL;
    obj->next = dest->head;
    obj->next->prev = obj;
    dest->head = obj;
  }

  dest->num_objects++;
}

void append_to_tail(doubly_linked_list* dest,
		    doubly_linked_list* src) {
  if(dest->tail == NULL) {
    dest->head = src->head;
    dest->tail = src->tail;
  } else if(src->head == NULL) {
    return;
  } else {
    /* link end of dest to start of src */
    dest->tail->next = src->head;
    dest->tail->next->prev = dest->tail;
    dest->tail = src->tail;
  }

  dest->num_objects += src->num_objects;

  src->head = NULL;
  src->tail = NULL;
  src->num_objects = 0;
}

/* these debug_* functions are far too slow to be called at normal
 * runtime but they're really useful for calling from gdb to make sure
 * my assumptions are holding at each step of garbage collection.
 */
#ifdef DEBUG_GC
long debug_list_contains(doubly_linked_list *list,
			 object *obj) {
  object *iter = list->head;
  if(iter == list->tail) {
    assert_gc(iter == obj, "object %p not in length1 list\n", obj);
    return 0;
  }

  long pos = 0;
  while(iter != list->tail) {
    if(iter == obj) {
      return pos;
    }
    iter = iter->next;
    ++pos;
  }
  assert_gc(iter == obj, "object %p not in list\n", obj);
  return list->num_objects - 1;
}


void debug_validate(doubly_linked_list *list) {
  /* verify the structure of a linked list */
  if(list->head == NULL || list->tail == NULL) {
    assert_gc(list->head == NULL &&
	      list->tail == NULL, "head and tail must be null together");
    assert_gc(list->num_objects == 0, "head is null. count != 0");
    return;
  }

  assert_gc(list->head->prev == NULL, "head's prev is not null");

  if(list->head == list->tail) {
    assert_gc(list->num_objects == 1, "1 length list invalid");
    assert_gc(list->head->next == NULL, "next of only item not null");
    assert_gc(list->tail->prev == NULL, "prev of only item not null");
    return;
  }

  assert_gc(list->head->next != NULL, "list head next is null");

  object *iter = list->head->next;
  object *last = NULL;

  long idx = 1;
  while(iter != list->tail) {
    assert_gc(iter->prev != NULL, "central node %ld prev is null",
	      idx);
    assert_gc(iter->next != NULL, "central node %ld next is null",
	      idx);
    if(last) {
      assert_gc(iter->prev == last,
		"central node %ld prev is wrong. Is %p. Should be %p",
		idx, iter->prev, last);
    }

    ++idx;

    last = iter;
    iter = iter->next;
  }

  ++idx;
  assert_gc(iter->next == NULL, "list tail next is not null");
  assert_gc(iter->prev != NULL, "list tail prev is null");
  assert_gc(iter->prev == last,
	    "list tail prev is wrong. Is %p. Should be %p",
	    iter->prev, last);
  assert_gc(idx == list->num_objects,
	    "list object count is wrong %ld != %ld",
	    idx, list->num_objects);
}
#else
#define debug_list_contains(a, b)
#define debug_validate(a)
#endif

typedef struct object_pointer_list {
  object ***objs;
  long top;
  long size;
} object_pointer_list;

static doubly_linked_list Active_Heap_Objects;
static doubly_linked_list Old_Heap_Objects;

static object *Next_Free_Object = NULL;
struct object_pointer_list *Root_Objects = NULL;

char current_color = 0;

void ensure_root_objects(void) {
  if(Root_Objects == NULL) {
    Root_Objects = MALLOC(sizeof(Root_Objects));
    Root_Objects->top = 0;
    Root_Objects->size = 400;
    Root_Objects->objs = MALLOC(sizeof(object **) * Root_Objects->size);
  }
}

void extend_heap(long);

void gc_init(void) {
  ensure_root_objects();

  Active_Heap_Objects.head = NULL;
  Active_Heap_Objects.tail = NULL;
  Active_Heap_Objects.num_objects = 0;

  Old_Heap_Objects.head = NULL;
  Old_Heap_Objects.tail = NULL;
  Old_Heap_Objects.num_objects = 0;

  extend_heap(1000);

  /* everything is free right now */
  Next_Free_Object = Active_Heap_Objects.head;
}

object *push_root(object ** stack) {
  /* grow the stack if we need to */
  if(Root_Objects->top == Root_Objects->size) {
    long new_size = Root_Objects->size * 2;
    Root_Objects->objs = realloc(Root_Objects->objs,
				 sizeof(object **) * new_size);
    Root_Objects->size = new_size;
    debug_gc("grew root stack to %ld objects\n", new_size);
  }

  Root_Objects->objs[Root_Objects->top++] = stack;
  return *stack;
}

void pop_root(object ** stack) {
  if(Root_Objects->objs[--Root_Objects->top] != stack) {
    /* scan back until we find it */
    int idx = Root_Objects->top - 1;
    object ** last = Root_Objects->objs[Root_Objects->top];
    int done = 0;
    for(;idx >= 0 && !done; --idx) {
      if(Root_Objects->objs[idx] == stack) {
	done = 1;
      }
      object ** temp = Root_Objects->objs[idx];
      Root_Objects->objs[idx] = last;
      last = temp;
    }
    if(!done) {
      print_backtrace();
      throw_gc("pop_stack_root - object not found\n");
    }
  }
}

/* extends the front of the heap. assumes the heap has already
 * been scavanged for any live objects
 */
void extend_heap(long extension) {
  int ii;
  object *new_heap = MALLOC(sizeof(object) * extension);

  new_heap[0].prev = NULL;
  new_heap[0].next = &new_heap[1];
  new_heap[0].color = current_color;

  for(ii = 1; ii < extension - 1; ++ii) {
    new_heap[ii].next = &new_heap[ii + 1];
    new_heap[ii].prev = &new_heap[ii - 1];
    new_heap[ii].color = current_color;
  }

  const long last = extension - 1;
  new_heap[last].next = Active_Heap_Objects.head;
  new_heap[last].prev = &new_heap[last - 1];

  if(Active_Heap_Objects.head) {
    Active_Heap_Objects.head->prev = &new_heap[last];
  } else {
    /* this is the first heap allocation */
    Active_Heap_Objects.tail = &new_heap[last];
  }
  new_heap[last].color = current_color;

  Active_Heap_Objects.head = new_heap;

  /* bump next free back */
  Next_Free_Object = new_heap;

  Active_Heap_Objects.num_objects += extension;
  debug_validate(&Active_Heap_Objects);
}

void move_reachable(object * root, doubly_linked_list *to_set) {
  int ii;
  hashtab_iter_t htab_iter;

  if(root == NULL)
    return;
  if(root->color == current_color)
    return;

  /* mark this and move it into the to_set */
  root->color = current_color;

  /* unlink from old list, maintaining the old lists
   * head and tail. */
  debug_list_contains(&Active_Heap_Objects, root);
  debug_validate(&Active_Heap_Objects);
  move_object_to_head(root, &Active_Heap_Objects, to_set);
  debug_list_contains(to_set, root);
  debug_validate(&Active_Heap_Objects);

  /* scan fields */
  switch (root->type) {
  case PAIR:
    move_reachable(car(root), to_set);
    move_reachable(cdr(root), to_set);
    break;
  case COMPOUND_PROC:
    move_reachable(root->data.compound_proc.env, to_set);
  case SYNTAX_PROC:
    move_reachable(root->data.compound_proc.parameters, to_set);
    move_reachable(root->data.compound_proc.body, to_set);
    break;
  case VECTOR:
    for(ii = 0; ii < VSIZE(root); ++ii) {
      move_reachable(VARRAY(root)[ii], to_set);
    }
    break;
  case COMPILED_PROC:
    move_reachable(BYTECODE(root), to_set);
    move_reachable(CENV(root), to_set);
    move_reachable(CIENV(root), to_set);
    break;
  case META_PROC:
    move_reachable(METAPROC(root), to_set);
    move_reachable(METADATA(root), to_set);
    break;
  case HASH_TABLE:
    htb_iter_init(HTAB(root), &htab_iter);
    while(htab_iter.key != NULL) {
      move_reachable((object *) htab_iter.key, to_set);
      move_reachable((object *) htab_iter.value, to_set);
      htb_iter_inc(&htab_iter);
    }
  default:
    break;
  }
}

void finalize_object(object *head) {
  /* free any extra memory associated with this type */
  switch (head->type) {
  case STRING:
    free(head->data.string.value);
    break;
  case VECTOR:
    free(VARRAY(head));
    break;
  case HASH_TABLE:
    htb_destroy(HTAB(head));
  default:
    break;
  }
}

long baker_collect() {
  /* merge everything into one big heap */
  append_to_tail(&Active_Heap_Objects, &Old_Heap_Objects);

  /* move everything reachable from a root into the old set */
  ++current_color;
  int ii = 0;
  for(ii = 0; ii < Root_Objects->top; ++ii) {
    object **next = Root_Objects->objs[ii];
    move_reachable(*next, &Old_Heap_Objects);
  }
  ++current_color;

  /* both sets should be valid */
  debug_validate(&Old_Heap_Objects);
  debug_validate(&Active_Heap_Objects);

  /* now everything left in Active is garbage and can be reused */
  Next_Free_Object = Active_Heap_Objects.head;
  long num_free = Active_Heap_Objects.num_objects;

  return num_free;
}

static long Alloc_Count = 0;
static long Next_Heap_Extension = 1000;

object *alloc_object(void) {
  /* always scavenge while we're debugging
     baker_collect();
   */

  if(Next_Free_Object == NULL) {
    debug_gc("no space. trying baker-collect\n");
    print_backtrace();

    /* comment this out to turn off gc
     */
    long freed = baker_collect();

    /* did we free enough? */
    if(freed == 0 || Next_Heap_Extension / freed > 2) {
      debug_gc("only freed %ld. extending the heap by %ld\n",
	       freed, Next_Heap_Extension);
      extend_heap(Next_Heap_Extension);
      Next_Heap_Extension *= 3;
    }

    if(Next_Free_Object == NULL) {
      throw_gc("extend_heap didn't work");
    }
  }

  object *obj = Next_Free_Object;
  obj->color = current_color;

  Next_Free_Object = Next_Free_Object->next;

  return obj;
}

long get_alloc_count() {
  return Alloc_Count;
}
