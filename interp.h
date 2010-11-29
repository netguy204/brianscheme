#ifndef INTERP_H
#define INTERP_H

#include "types.h"
#include "symbols.h"

/* pre-declarations */

object *cons(object *car, object *cdr);
object *car(object *pair);
object *cdr(object *pair);
#define DEFUN1(name) object* name(object *arguments, \
				  object *environment)
#define FIRST car(arguments)
#define SECOND cadr(arguments)
#define NEXT arguments = cdr(arguments)

#define AS_BOOL(x) (x ? true : false)
#define LONG(x) x->data.fixnum.value
#define CHAR(x) x->data.character.value
#define STRING(x) x->data.string.value

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
object *lookup_variable_value(object *var, object *env);
void set_variable_value(object *var, object *new_val,
			object *env);
void define_variable(object *var, object *new_val,
		     object *env);
object *setup_environment(void);
void init_prim_environment(object *env);
void init();

void write(FILE *out, object *obj);
object *expand_macro(object *macro, object *args, object *env);
object *interp(object *exp, object *env);
object *interp1(object *exp, object *env, int level);
void debug_write(char * msg, object *obj, int level);
void print_obj(object *obj);

#endif
