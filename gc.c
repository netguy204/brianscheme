#include <stdlib.h>
#include <stdarg.h>
#include <execinfo.h>

#include "types.h"
#include "symbols.h"

void *MALLOC(long size) {
  void *obj = malloc(size);
  if(obj == NULL) {
    fprintf(stderr, "out of memory\n");
    exit(1);
  }
  return obj;
}

typedef struct heap_list {
  object *heap;
  long size;
  struct heap_list *next;
} heap_list;

typedef struct object_list {
  object **obj;
  struct object_list *next;
} object_list;

/* initialized to the_empty_list */
static object* Free_Objects = NULL;
static object_list* Root_Objects = NULL;
static heap_list* Active_Heaps = NULL;
static object* Active_List = NULL;

void throw_gc(char *msg) {
  fprintf(stderr, "gc: %s", msg);
  exit(2);
}

void print_backtrace() {
#define MAX_FRAMES 30
  void *buffer[MAX_FRAMES];
  int frames = backtrace(buffer, MAX_FRAMES);
  backtrace_symbols_fd(buffer, frames, 2);
}

object *push_root(object **stack) {
  object_list *new_anchor = MALLOC(sizeof(object_list));
  new_anchor->next = Root_Objects;
  new_anchor->obj = stack;
  Root_Objects = new_anchor;
  return *stack;
}

void pop_root(object **stack) {
  if(Root_Objects->obj != stack) {
    throw_gc("pop_stack_root - object not on top\n");
  }

  object_list *old_anchor = Root_Objects;
  Root_Objects = Root_Objects->next;
  free(old_anchor);
}


void debug_gc(char *msg, ...) {
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
}

void extend_heap(long extension) {
  int ii;
  object *new_heap = MALLOC(sizeof(object) * extension);

  for(ii = 0; ii < extension-1; ++ii) {
    new_heap[ii].next = &new_heap[ii+1];
  }

  new_heap[extension-1].next = Free_Objects;
  Free_Objects = new_heap;

  /* add this new heap to the heap list */
  heap_list *heap_pointer = MALLOC(sizeof(heap_list));
  heap_pointer->heap = new_heap;
  heap_pointer->size = extension;
  heap_pointer->next = Active_Heaps;
  Active_Heaps = heap_pointer;
}

void mark_reachable(object *root) {
  if(root == NULL) return;
  if(root->mark) return;

  root->mark = 1;

  switch(root->type) {
  case PAIR:
    mark_reachable(car(root));
    mark_reachable(cdr(root));
    break;
  case COMPOUND_PROC:
  case SYNTAX_PROC:
    mark_reachable(root->data.compound_proc.parameters);
    mark_reachable(root->data.compound_proc.body);
    mark_reachable(root->data.compound_proc.env);
    break;
  default:
    break;
  }
}

long sweep_unmarked() {
  long num_freed = 0;
  object *head = Active_List;
  object *next_head;

  while(head) {
    next_head = head->next;

    /* unreached so free it */
    if(!head->mark) {
      if(head->type == STRING) {
	free(head->data.string.value);
      }
      
      head->next = Free_Objects;
      Free_Objects = head;
      num_freed++;
    }

    head->mark = 0;
    head = next_head;
  }

  return num_freed;
}

long mark_and_sweep() {
  /* mark everything reachable from root */
  object_list *next = Root_Objects;
  while(next) {
    debug_gc("examining root %ld\n", next);
    if(next->obj) {
      mark_reachable(*(next->obj));
    } else {
      debug_gc("root is null\n");
    }
    next = next->next;
  }

  return sweep_unmarked();
}

static long Alloc_Count = 0;
static long Next_Heap_Extension = 1000;

object *alloc_object(void) {
  if(Free_Objects == NULL) {
    debug_gc("no space. trying mark-and-sweep\n");
    print_backtrace();

    long freed = mark_and_sweep();
    Alloc_Count -= freed;
    debug_gc("mark-and-sweep freed %ld objects\n", freed);
    debug_gc("alloc-count is now %ld\n", Alloc_Count);


    /* did we reclaim anything? */
    if(Free_Objects == NULL) {
      debug_gc("extending the heap\n");
      extend_heap(Next_Heap_Extension);
      Next_Heap_Extension *= 1.8;
    }

    if(Free_Objects == NULL) {
      throw_gc("extend_heap didn't work");
    }
  }

  object *obj = Free_Objects;
  Free_Objects = obj->next;

  obj->next = Active_List;
  Active_List = obj;

  ++Alloc_Count;
  return obj;
}

long get_alloc_count() {
  return Alloc_Count;
}

