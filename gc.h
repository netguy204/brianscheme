#ifndef GC_H
#define GC_H

long mark_and_sweep();
object *push_root(object **stack);
void pop_root(object **stack);

object *alloc_object(void);
long get_alloc_count();

#endif
