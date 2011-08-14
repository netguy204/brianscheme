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
void vm_add_roots(void);
void vm_boot(void);
void vm_init_environment(definer defn);

object *vm_execute(object *fn, fancystack *stack, long n_args, object *genv);

void vm_definer(char *sym, object *value);

#define VPUSH(obj, stack)				\
  do {							\
    stack->data[stack->top++] = obj;			\
  } while(0)

#define VPOP(tgt, stack)			\
  do {						\
    tgt = stack->data[--stack->top];		\
  } while(0)


#endif
