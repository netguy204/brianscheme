#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "symbols.h"
#include "hashtab.h"

/* enable gc debuging by defining
 * DEBUG_GC
 */
#define DEBUG_GC

#ifdef DEBUG_GC
void debug_gc(char *msg, ...) {
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
}
#else
#define debug_gc(msg, ...)
#endif

void *MALLOC(long size) {
  debug_gc("malloc %ld\n", size);
  void *obj = malloc(size);
  if(obj == NULL) {
    fprintf(stderr, "out of memory\n");
    exit(1);
  }
  return obj;
}

typedef struct root_stack {
  object ***objs;
  long top;
  long size;
} root_stack;

typedef struct object_heap {
  object *objs;
  long free_idx;
  long size;
} object_heap;

/* initialized to the_empty_list */
static struct root_stack *Root_Objects = NULL;
static struct object_heap* active_objects = NULL;
static struct object_heap* other_objects = NULL;

void throw_gc(char *msg, ...) {
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);

  exit(2);
}

object_heap* create_heap(long size) {
  object_heap * heap = MALLOC(sizeof(object_heap));
  heap->objs = MALLOC(sizeof(object) * size);
  heap->free_idx = 0;
  heap->size = size;

  return heap;
}

void gc_init(void) {
  debug_gc("initializing gc\n");

  /* build the intial root stack */
  Root_Objects = MALLOC(sizeof(Root_Objects));
  Root_Objects->top = 0;
  Root_Objects->size = 400;
  Root_Objects->objs = MALLOC(sizeof(object **) * Root_Objects->size);

  /* allocate the first active heaps */
  active_objects = create_heap(1024);
  other_objects = create_heap(1024);
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
    throw_gc("pop_stack_root - object not on top\n");
  }
}

void cheney_copy_root(object ** root) {
  int ii;
  hashtab_iter_t htab_iter;

  if(root == NULL)
    return;

  /* has this been relocated yet? */
  int new_idx = (*root)->new_object_location - other_objects->objs;
  if(new_idx >= 0 && new_idx < other_objects->size) {
    debug_gc("encountered already migrated object\n");
    *root = (*root)->new_object_location;
    return;
  }

  debug_gc("migrating object\n");
  /* move it, set the location, recurse */
  object * new_address = &(other_objects->objs[other_objects->free_idx++]);
  (*root)->new_object_location = new_address;
  memcpy(new_address, *root, sizeof(object));
  *root = new_address;

  switch ((*root)->type) {
  case PAIR:
    cheney_copy_root(&CAR(*root));
    cheney_copy_root(&CDR(*root));
    break;
  case COMPOUND_PROC:
    cheney_copy_root(&LENV(*root));
  case SYNTAX_PROC:
    cheney_copy_root(&LARGS(*root));
    cheney_copy_root(&LBODY(*root));
    break;
  case VECTOR:
    for(ii = 0; ii < VSIZE((*root)); ++ii) {
      cheney_copy_root(&(VARRAY((*root))[ii]));
    }
    break;
  case COMPILED_PROC:
    cheney_copy_root(&(BYTECODE(*root)));
    cheney_copy_root(&(CENV(*root)));
    break;
  case HASH_TABLE:
    ht_iter_init(HTAB(*root), &htab_iter);
    while(htab_iter.key != NULL) {
      cheney_copy_root(htab_iter.key);
      cheney_copy_root(htab_iter.value);
      ht_iter_inc(&htab_iter);
    }
  default:
    break;
  }
}

void cheney_copy() {
  /* mark everything reachable from root */
  int ii = 0;
  for(ii = 0; ii < Root_Objects->top; ++ii) {
    object **next = Root_Objects->objs[ii];
    cheney_copy_root(next);
  }
}

object *alloc_object(void) {

  if(active_objects->free_idx == active_objects->size) {
    debug_gc("starting cheney copy\n");
    cheney_copy();

    /* swap the heaps */
    object_heap *temp = active_objects;
    active_objects = other_objects;
    other_objects = temp;
    other_objects->free_idx = 0;

    /* grow the other heap if necessary */
    long unused = other_objects->size - other_objects->free_idx;
    double p_full = ((double)unused) / ((double)other_objects->size);
    if(p_full > 0.80) {
      free(other_objects->objs);
      other_objects->size *= 2;
      debug_gc("resizing other heap to %ld\n", other_objects->size);
      other_objects = MALLOC(sizeof(object) * other_objects->size);
    }

    /* can we satisfy the current allocation? */
    if(active_objects->free_idx == active_objects->size) {
      return alloc_object();
    }
  }
  debug_gc("allocating at index %d\n", active_objects->free_idx);
  return &(active_objects->objs[active_objects->free_idx++]);
}

