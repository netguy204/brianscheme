#ifndef FANCYSTACK_H
#define FANCYSTACK_H

struct fancystack_t {
  long top;
  long size;
  void **data;
  struct fancystack_t *next;
};

typedef struct fancystack_t fancystack;

void fancystack_init();

fancystack *fancystack_alloc(int pages);

void fancystack_free(fancystack * stack);

void fancystack_cleanup();

void fancystack_push(fancystack * stack, void *object);

void *fancystack_peek(fancystack * stack);

void *fancystack_pop(fancystack * stack);

fancystack *fancystack_first();

#endif
