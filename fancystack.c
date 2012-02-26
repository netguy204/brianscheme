#include <signal.h>
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/mman.h>
#include <unistd.h>

#include "fancystack.h"
static long pagesize;
static fancystack *stack_list = NULL;
static fancystack *faulting_stack = NULL;

static void fancystack_handler(int sig, siginfo_t *si, void* data);

void fancystack_init() {
  pagesize = getpagesize();
  faulting_stack = NULL;

  // set up the handler
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = fancystack_handler;
#ifdef __MACH__
  if(sigaction(SIGBUS, &sa, NULL) == -1) {
#else
  if(sigaction(SIGSEGV, &sa, NULL) == -1) {
#endif
    fprintf(stderr, "failed to install signal handler\n");
    exit(1);
  }
}

void *fancystack_guard(fancystack* stack) {
  return (void*)((char*)stack->data + stack->size);
}

fancystack *fancystack_alloc(int pages) {
  // allocate one extra page so that we can detect overflow
  long size = pagesize * pages;
#ifdef __MACH__
  void *stack_data = malloc(size + pagesize);
#else
  void *stack_data = memalign(pagesize, size + pagesize);
#endif
  
  // allocate the stack record
  fancystack *stack = malloc(sizeof(fancystack));
  stack->data = stack_data;
  stack->top = 0;
  stack->size = pagesize * pages;
  stack->next = stack_list;
  stack_list = stack;

  // write protect that extra page
  if(mprotect(fancystack_guard(stack), pagesize, PROT_READ) == -1) {
    fprintf(stderr, "failed to create stack protect page\n");
    exit(1);
  }
  
  return stack;
}

void fancystack_free(fancystack *stack) {
  if(stack_list == stack) {
    stack_list = stack_list->next;
    free(stack);
    return;
  }

  fancystack *current = stack_list;
  while(current->next != NULL) {
    if(current->next == stack) {
      current->next = stack->next;
      free(stack);
      return;
    }
  }

  fprintf(stderr, "couldn't find fancystack to free\n");
  exit(1);
}

void fancystack_grow(fancystack *stack) {
  // always double the size
  long new_size = stack->size * 2;
#ifdef __MACH__
  void *new_data = malloc(new_size + pagesize);
#else
  void *new_data = memalign(pagesize, new_size + pagesize);
#endif

  memcpy(new_data, stack->data, stack->size + pagesize);
  free(stack->data);

  stack->data = new_data;
  stack->size = new_size;

  if(mprotect(fancystack_guard(stack), pagesize, PROT_READ) == -1) {
    fprintf(stderr, "failed to create stack protect page\n");
    exit(1);
  }
}

fancystack *fancystack_faulting(void *fault_ptr) {
  fancystack *current = stack_list;
  while(current) {
    void *guard_start = fancystack_guard(current);
    //fprintf(stderr, "checking guardpage %p - %p\n", guard_start, guard_start + pagesize);
    if(fault_ptr >= guard_start && fault_ptr < guard_start + pagesize) {
      return current;
    }
    current = current->next;
  }
  return NULL;
}

static void fancystack_handler(int sig, siginfo_t *si, void* data) {
  // find the faulting stack and unprotect it. Note: this can only
  // handle one stack fault per cleanup cycle
  faulting_stack = fancystack_faulting(si->si_addr);

  if(faulting_stack == NULL) {
    fprintf(stderr, "unexpected SIGBUS, faultptr = %p\n", si->si_addr);
    exit(1);
  }

  // make the memory readwrite so work can continue
  void *guard_page = fancystack_guard(faulting_stack);
  if(mprotect(guard_page, pagesize, PROT_READ | PROT_WRITE) == -1) {
    fprintf(stderr, "failed to read/write protect faulting stack\n");
    exit(1);
  }
}

void fancystack_cleanup() {
  if(faulting_stack != NULL) {
    fancystack_grow(faulting_stack);
    faulting_stack = NULL;
  }
}

void fancystack_push(fancystack* stack, void *object) {
  stack->data[stack->top++] = object;
}

void *fancystack_peek(fancystack* stack) {
  return stack->data[stack->top - 1];
}

void *fancystack_pop(fancystack* stack) {
  stack->top -= 1;
  return stack->data[stack->top];
}

fancystack *fancystack_first() {
  return stack_list;
}

static int test() {
  fancystack_init();

  fancystack* stack = fancystack_alloc(1);
  int outer;

  // this should fault and grow the stack several times
  for(outer = 0; outer < 4096; ++outer) {
    fancystack_cleanup();
    //printf("step %d\n", outer);

    int inner;
    for(inner = 0; inner < 256; ++inner) {
      fancystack_push(stack, NULL);
    }
  }

  printf("stack size is now %ld\n", stack->size);

  return 0;
}
