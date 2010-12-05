#ifndef VM_H
#define VM_H

#include "types.h"

void vm_init(void);
object *vm_execute(object *fn, object *stack);

#endif
