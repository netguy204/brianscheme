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

#ifndef INTERP_H
#define INTERP_H

#include "types.h"
#include "symbols.h"

/* pre-declarations */

object *cons(object *car, object *cdr);
object *car(object *pair);
object *cdr(object *pair);
#define DEFUN1(name) \
  object* name(object *arguments __attribute__ ((unused)),	\
	       object *environment __attribute__ ((unused)))

#define FIRST car(arguments)
#define SECOND second(arguments)
#define THIRD third(arguments)
#define FOURTH fourth(arguments)

#define NEXT arguments = cdr(arguments)

#define AS_BOOL(x) (x ? true : false)

/* environments */
object *enclosing_environment(object *env);
object *first_frame(object *env);
object *make_frame(object *variables, object *values);
object *frame_variables(object *frame);
object *frame_values(object *frame);
void add_binding_to_frame(object *var, object *val,
			  object *frame);
object *extend_environment(object *vars, object *vals,
			   object *base_env);
object *lookup_global_value(object *var, object *env);
void define_global_variable(object *var, object *val, object *env);
object *lookup_variable_value(object *var, object *env);
void set_variable_value(object *var, object *new_val,
			object *env);
void define_variable(object *var, object *new_val,
		     object *env);
void init_prim_environment(object * env);
void init();

void owrite(FILE *out, object *obj);
char is_falselike(object *obj);
object *expand_macro(object *macro, object *args,
		     object *env, int level);
object *interp(object *exp, object *env);
object *interp1(object *exp, object *env, int level);
object *debug_write(char * msg, object *obj, int level);
void print_obj(object *obj);

#endif
