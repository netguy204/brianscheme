#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "symbols.h"

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

typedef struct object_pointer_list {
  object ***objs;
  long top;
  long size;
} object_list;

/* initialized to the_empty_list */
static object *Free_Objects = NULL;
struct object_pointer_list *Root_Objects = NULL;
static object *Active_List = NULL;

void throw_gc(char *msg) {
  fprintf(stderr, "gc: %s", msg);
  exit(2);
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
#else
void print_backtrace() {}
void debug_gc(char *msg, ...) {}
#endif

void ensure_root_objects(void) {
  if(Root_Objects == NULL) {
    Root_Objects = MALLOC(sizeof(Root_Objects));
    Root_Objects->top = 0;
    Root_Objects->size = 200;
    Root_Objects->objs = MALLOC(sizeof(object**) * 
				Root_Objects->size);
  }
}

object *push_root(object **stack) {
  /* grow the stack if we need to */

  /* FIXME: why doesn't this work?
  if(Root_Objects->top == Root_Objects->size) {
    long new_size = Root_Objects->size * 2 + 100;
    debug_gc("growing root stack to %ld\n", new_size);
    Root_Objects->objs = realloc(Root_Objects->objs, new_size);
    Root_Objects->size = new_size;
  }
  */

  Root_Objects->objs[Root_Objects->top++] = stack;
  return *stack;
}

void pop_root(object **stack) {
  if(Root_Objects->objs[--Root_Objects->top] != stack) {
    print_backtrace();
    throw_gc("pop_stack_root - object not on top\n");
  }
}

void extend_heap(long extension) {
  int ii;
  object *new_heap = MALLOC(sizeof(object) * extension);

  for(ii = 0; ii < extension-1; ++ii) {
    new_heap[ii].next = &new_heap[ii+1];
  }

  new_heap[extension-1].next = Free_Objects;
  Free_Objects = new_heap;
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
  object *new_active = NULL;

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
    } else {
      head->next = new_active;
      new_active = head;
    }

    head->mark = 0;
    head = next_head;
  }

  Active_List = new_active;

  return num_freed;
}

long mark_and_sweep() {
  ensure_root_objects();

  /* mark everything reachable from root */
  int ii = 0;
  for(ii = 0; ii < Root_Objects->top; ++ii) {
    object **next = Root_Objects->objs[ii];
    mark_reachable(*next);
  }

  return sweep_unmarked();
}

static long Alloc_Count = 0;
static long Next_Heap_Extension = 1000;

object *alloc_object(void) {
  /* always sweep while we're debugging
  mark_and_sweep();
  */

  if(Free_Objects == NULL) {
    debug_gc("no space. trying mark-and-sweep\n");
    print_backtrace();

    long freed = mark_and_sweep();
    Alloc_Count -= freed;
    debug_gc("mark-and-sweep freed %ld objects\n", freed);
    debug_gc("alloc-count is now %ld\n", Alloc_Count);


    /* did we free enough? */
    if(freed == 0 || Next_Heap_Extension / freed > 10) {
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

  /* clear when we're debugging so things fail
   * quickly 
  memset(obj, 0, sizeof(object));
   */

  obj->next = Active_List;
  Active_List = obj;

  ++Alloc_Count;

  return obj;
}

long get_alloc_count() {
  return Alloc_Count;
}
