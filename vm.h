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

#ifndef VM_H
#define VM_H

#include "types.h"

void vm_init(void);
void vm_init_environment(object *env);

object *vm_execute(object *fn, object *stack, long stack_top, long n_args);

void vector_push(object *stack, object *obj, long top);

object *vector_pop(object *stack, long top);


#define VPUSH(obj, stack, top)				\
  do {							\
    vector_push(stack, obj, top);			\
    ++top;						\
  } while(0)

#define VPOP(tgt, stack, top)			\
  do {						\
    tgt = vector_pop(stack, top);		\
    --top;					\
  } while(0)


#endif
