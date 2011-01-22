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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>

#include "types.h"
#include "interp.h"
#include "read.h"
#include "gc.h"
#include "vm.h"

static const int DEBUG_LEVEL = 1;
static char debug_enabled = 0;

void eval_exit_hook(object * environment) {
  object *exit_hook = lookup_variable_value(exit_hook_symbol,
					    environment);
  if(exit_hook != the_empty_list) {
    object *exp = cons(exit_hook, the_empty_list);
    push_root(&exp);
    interp(exp, environment);
    pop_root(&exp);
  }
}

void throw_interp(char *msg, ...) {
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);

  eval_exit_hook(the_global_environment);
  exit(1);
}

/* dealing with environments */
object *enclosing_environment(object * env) {
  return cdr(env);
}

object *first_frame(object * env) {
  return car(env);
}

object *make_frame(object * variables, object * values) {
  return cons(variables, values);
}

object *frame_variables(object * frame) {
  return car(frame);
}

object *frame_values(object * frame) {
  return cdr(frame);
}

void add_binding_to_frame(object * var, object * val, object * frame) {
  set_car(frame, cons(var, car(frame)));
  set_cdr(frame, cons(val, cdr(frame)));
}

object *extend_environment(object * vars, object * vals, object * base_env) {
  object *result;

  result = make_frame(vars, vals);
  push_root(&result);
  result = cons(result, base_env);
  pop_root(&result);

  return result;
}

object *lookup_variable_value(object * var, object * env) {
  object *frame;
  object *vars;
  object *vals;

  while(!is_the_empty_list(env)) {
    frame = first_frame(env);

    if(is_hashtab(frame)) {
      object *res = get_hashtab(frame, var, NULL);
      if(res != NULL) {
	return res;
      }
    }
    else {
      vars = frame_variables(frame);
      vals = frame_values(frame);

      /* handles (lambda foo ...) */
      if(var == vars) {
	return vals;
      }

      while(is_pair(vars)) {
	if(var == car(vars)) {
	  return car(vals);
	}
	/* handles (lambda ( foo . rest ) ..) */
	if(var == cdr(vars)) {
	  return cdr(vals);
	}

	vars = cdr(vars);

	/* since these may not be the same length
	 * we need to check again */
	if(!is_the_empty_list(vals)) {
	  vals = cdr(vals);
	}
      }
    }
    env = enclosing_environment(env);
  }

  throw_interp("lookup failed. variable %s is unbound\n", STRING(var));
  return NULL;
}

void define_global_variable(object * var, object * new_val, object * env) {
  while(!is_the_empty_list(enclosing_environment(env))) {
    env = enclosing_environment(env);
  }

  object *frame = first_frame(env);
  if(is_hashtab(frame)) {
    set_hashtab(frame, var, new_val);
  }
  else {
    add_binding_to_frame(var, new_val, frame);
  }
}

/**
 * like set_variable_value but we define the variable if it doesn't
 * exist in a reachable frame.
 */
void define_variable(object * var, object * new_val, object * env) {
  object *frame;
  object *vars;
  object *vals;

  object *senv = env;
  while(!is_the_empty_list(senv)) {
    frame = first_frame(senv);
    if(is_hashtab(frame)) {
      object *obj = get_hashtab(frame, var, NULL);
      if(obj != NULL) {
	set_hashtab(frame, var, new_val);
	return;
      }
    }
    else {
      vars = frame_variables(frame);
      vals = frame_values(frame);
      while(is_pair(vars)) {
	if(var == car(vars)) {
	  set_car(vals, new_val);
	  return;
	}
	if(var == cdr(vars)) {
	  set_cdr(vals, new_val);
	  return;
	}
	vars = cdr(vars);

	/* since these may not be the same length
	 * we need to check again */
	if(!is_the_empty_list(vals)) {
	  vals = cdr(vals);
	}
      }
    }
    senv = enclosing_environment(senv);
  }

  /* we define at the global level */
  define_global_variable(var, new_val, env);
}

/**
 * create a complete empty and unrooted environment
 */
object *setup_environment(void) {
  object *table = make_hashtab(100);
  push_root(&table);
  object *env = cons(table, the_empty_environment);
  pop_root(&table);
  return env;
}


/* define a few primitives */

DEFUN1(is_null_proc) {
  return AS_BOOL(is_the_empty_list(FIRST));
}

DEFUN1(is_boolean_proc) {
  return AS_BOOL(is_boolean(FIRST));
}

DEFUN1(is_symbol_proc) {
  return AS_BOOL(is_symbol(FIRST));
}

DEFUN1(is_integer_proc) {
  return AS_BOOL(is_fixnum(FIRST));
}

DEFUN1(is_real_proc) {
  return AS_BOOL(is_real(FIRST));
}

DEFUN1(is_char_proc) {
  return AS_BOOL(is_character(FIRST));
}

DEFUN1(is_string_proc) {
  return AS_BOOL(is_string(FIRST));
}

DEFUN1(is_pair_proc) {
  return AS_BOOL(is_pair(FIRST));
}

DEFUN1(is_procedure_proc) {
  return AS_BOOL(is_primitive_proc(FIRST) || is_compound_proc(FIRST) ||
		 is_compiled_proc(FIRST) ||
		 (is_meta(FIRST) &&
		  is_procedure_proc(METAPROC(FIRST), environment)));
}

DEFUN1(is_compound_proc_proc) {
  return AS_BOOL(is_compound_proc(FIRST));
}

DEFUN1(is_syntax_proc_proc) {
  return AS_BOOL(is_syntax_proc(FIRST));
}

DEFUN1(is_output_port_proc) {
  return AS_BOOL(is_output_port(FIRST));
}

DEFUN1(is_input_port_proc) {
  return AS_BOOL(is_input_port(FIRST));
}

DEFUN1(is_alien_proc) {
  return AS_BOOL(is_alien(FIRST));
}

DEFUN1(is_eof_proc) {
  return AS_BOOL(is_eof_object(FIRST));
}

DEFUN1(is_compiled_proc_proc) {
  return AS_BOOL(is_compiled_proc(FIRST));
}

DEFUN1(add_fixnum_proc) {
  long result = 0;
  while(!is_the_empty_list(arguments)) {
    result += LONG(FIRST);
    NEXT;
  }
  return make_fixnum(result);
}

DEFUN1(add_real_proc) {
  double result = 0;
  while(!is_the_empty_list(arguments)) {
    result += DOUBLE(FIRST);
    NEXT;
  }
  return make_real(result);
}

DEFUN1(sub_fixnum_proc) {
  long result = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result -= LONG(FIRST);
  }
  return make_fixnum(result);
}

DEFUN1(sub_real_proc) {
  double result = DOUBLE(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result -= DOUBLE(FIRST);
  }
  return make_real(result);
}

DEFUN1(mul_fixnum_proc) {
  long result = 1;
  while(!is_the_empty_list(arguments)) {
    result *= LONG(FIRST);
    NEXT;
  }
  return make_fixnum(result);
}

DEFUN1(mul_real_proc) {
  double result = 1;
  while(!is_the_empty_list(arguments)) {
    result *= DOUBLE(FIRST);
    NEXT;
  }
  return make_real(result);
}

DEFUN1(div_fixnum_proc) {
  long result = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result /= LONG(FIRST);
  }
  return make_fixnum(result);
}

DEFUN1(div_real_proc) {
  double result = DOUBLE(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result /= DOUBLE(FIRST);
  }
  return make_real(result);
}

DEFUN1(mod_fixnum_proc) {
  long result = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result %= LONG(FIRST);
  }
  return make_fixnum(result);
}

DEFUN1(mod_real_proc) {
  double result = DOUBLE(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result = fmod(result, DOUBLE(FIRST));
  }
  return make_real(result);
}

DEFUN1(pow_fixnum_proc) {
  long result = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result = pow(result, LONG(FIRST));
  }
  return make_fixnum(result);
}

DEFUN1(pow_real_proc) {
  double result = DOUBLE(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result = pow(result, DOUBLE(FIRST));
  }
  return make_real(result);
}

DEFUN1(logand_proc) {
  long result = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result &= LONG(FIRST);
  }
  return make_fixnum(result);
}

DEFUN1(logor_proc) {
  long result = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result |= LONG(FIRST);
  }
  return make_fixnum(result);
}

DEFUN1(logxor_proc) {
  long result = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result ^= LONG(FIRST);
  }
  return make_fixnum(result);
}

DEFUN1(ash_proc) {
  unsigned long result = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    long n = LONG(FIRST);
    if (n > 0)
      result <<= n;
    else
      result >>= -n;
  }
  return make_fixnum(result);
}

DEFUN1(sqrt_fixnum_proc) {
  long input = LONG(FIRST);
  double result = sqrt(input);
  return make_real(result);
}

DEFUN1(sqrt_real_proc) {
  double input = DOUBLE(FIRST);
  double result = sqrt(input);
  return make_real(result);
}

DEFUN1(log_fixnum_proc) {
  long input = LONG(FIRST);
  double result = log(input);
  return make_real(result);
}

DEFUN1(log_real_proc) {
  double input = DOUBLE(FIRST);
  double result = log(input);
  return make_real(result);
}

DEFUN1(floor_proc) {
  return make_fixnum((long)floor(DOUBLE(FIRST)));
}

DEFUN1(ceil_proc) {
  return make_fixnum((long)ceil(DOUBLE(FIRST)));
}

DEFUN1(round_proc) {
  return make_fixnum((long)round(DOUBLE(FIRST)));
}

DEFUN1(fixnum_to_real_proc) {
  return make_real((double)LONG(FIRST));
}

DEFUN1(debug_proc) {
  if(FIRST == false) {
    debug_enabled = 0;
  }
  else {
    debug_enabled = 1;
  }
  return FIRST;
}

DEFUN1(cons_proc) {
  return cons(FIRST, SECOND);
}

DEFUN1(car_proc) {
  return car(FIRST);
}

DEFUN1(cdr_proc) {
  return cdr(FIRST);
}

DEFUN1(set_car_proc) {
  set_car(FIRST, SECOND);
  return SECOND;
}

DEFUN1(set_cdr_proc) {
  set_cdr(FIRST, SECOND);
  return SECOND;
}

DEFUN1(list_proc) {
  return arguments;
}

DEFUN1(is_eq_proc) {
  if(FIRST->type != SECOND->type) {
    return false;
  }
  switch (FIRST->type) {
  case FIXNUM:
    return (LONG(FIRST) == LONG(SECOND)) ? true : false;
  case CHARACTER:
    return (CHAR(FIRST) == CHAR(SECOND)) ? true : false;
  case STRING:
    return (strcmp(STRING(FIRST), STRING(SECOND)) == 0) ? true : false;
  default:
    return (FIRST == SECOND) ? true : false;
  }
}

DEFUN1(is_number_equal_fixnum_proc) {
  long value = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    if(value != LONG(FIRST)) {
      return false;
    }
  }
  return true;
}

DEFUN1(is_number_equal_real_proc) {
  double value = DOUBLE(FIRST);
  while(!is_the_empty_list(NEXT)) {
    if(value != DOUBLE(FIRST)) {
      return false;
    }
  }
  return true;
}

DEFUN1(is_less_than_fixnum_proc) {
  long last = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    if(last < LONG(FIRST)) {
      last = LONG(FIRST);
    }
    else {
      return false;
    }
  }
  return true;
}

DEFUN1(is_less_than_real_proc) {
  double last = DOUBLE(FIRST);
  while(!is_the_empty_list(NEXT)) {
    if(last < DOUBLE(FIRST)) {
      last = DOUBLE(FIRST);
    }
    else {
      return false;
    }
  }
  return true;
}

DEFUN1(is_greater_than_fixnum_proc) {
  long last = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    if(last > LONG(FIRST)) {
      last = LONG(FIRST);
    }
    else {
      return false;
    }
  }
  return true;
}

DEFUN1(is_greater_than_real_proc) {
  double last = DOUBLE(FIRST);
  while(!is_the_empty_list(NEXT)) {
    if(last > DOUBLE(FIRST)) {
      last = DOUBLE(FIRST);
    }
    else {
      return false;
    }
  }
  return true;
}

DEFUN1(open_output_port_proc) {
  object *name = FIRST;
  FILE *out = fopen(STRING(name), "w");
  if(out == NULL) {
    return eof_object;
  }
  return make_output_port(out);
}

DEFUN1(close_output_port_proc) {
  FILE *out = OUTPUT(FIRST);
  fclose(out);
  return true;
}

DEFUN1(open_input_port_proc) {
  object *name = FIRST;
  FILE *in = fopen(STRING(name), "r");
  if(in == NULL) {
    return eof_object;
  }
  return make_input_port(in);
}

DEFUN1(close_input_port_proc) {
  FILE *in = INPUT(FIRST);
  fclose(in);
  return true;
}

DEFUN1(eval_proc) {
  object *exp = FIRST;
  object *env;

  if(cdr(arguments) == the_empty_list) {
    env = environment;
  }
  else {
    env = SECOND;
  }

  return interp(exp, env);
}

DEFUN1(apply_proc) {
  object *fn = FIRST;
  object *evald_args = SECOND;

  /* essentially duplicated from interp but I'm not
   * sure how to implement this properly otherwise.*/
  object *env;
  object *exp;
  object *result;

  /* unwrap meta */
  if(is_meta(fn)) {
    fn = METAPROC(fn);
  }

  if(is_primitive_proc(fn)) {
    return fn->data.primitive_proc.fn(evald_args, environment);
  }
  else if(is_compound_proc(fn)) {
    env = extend_environment(fn->data.compound_proc.parameters,
			     evald_args, fn->data.compound_proc.env);
    push_root(&env);
    exp = fn->data.compound_proc.body;
    result = interp(exp, env);
    pop_root(&env);
    return result;
  }
  else if(is_compiled_proc(fn)) {
    /* need to reverse the arguments */
    object *stack = make_vector(the_empty_list, 30);
    long stack_top = 0;

    push_root(&stack);

    while(!is_the_empty_list(evald_args)) {
      VARRAY(stack)[stack_top++] = car(evald_args);
      evald_args = cdr(evald_args);
    }

    result = vm_execute(fn, stack, stack_top);
    pop_root(&stack);
    return result;
  }

  throw_interp("\ncannot apply non-function\n");
  return false;
}

object *obj_read(FILE * in);
DEFUN1(read_proc) {
  object *in_port = FIRST;
  object *result = obj_read(INPUT(in_port));
  return (result == NULL) ? eof_object : result;
}

DEFUN1(write_proc) {
  object *port = SECOND;
  object *obj = FIRST;
  owrite(OUTPUT(port), obj);
  return true;
}

DEFUN1(write_char_proc) {
  object *port = SECOND;
  object *ch = FIRST;
  putc(CHAR(ch), OUTPUT(port));
  return true;
}

DEFUN1(read_char_proc) {
  object *port = FIRST;
  int result = getc(INPUT(port));
  return (result == EOF) ? eof_object : make_character(result);
}

DEFUN1(unread_char_proc) {
  object *port = FIRST;
  object *ch = SECOND;

  ungetc(CHAR(ch), INPUT(port));
  return true;
}

DEFUN1(char_to_integer_proc) {
  return make_fixnum(CHAR(FIRST));
}

DEFUN1(integer_to_char_proc) {
  return make_character((char)LONG(FIRST));
}

DEFUN1(make_string_proc) {
  int idx;
  long length = LONG(FIRST) + 1;
  object *fill = SECOND;
  char fill_char = '\0';
  if(is_character(fill)) {
    fill_char = CHAR(fill);
  }

  object *string = make_empty_string(length);
  for(idx = 0; idx < length - 1; ++idx) {
    STRING(string)[idx] = fill_char;
  }

  STRING(string)[length - 1] = '\0';
  return string;
}

DEFUN1(string_ref_proc) {
  return make_character(STRING(FIRST)[LONG(SECOND)]);
}

DEFUN1(string_set_proc) {
  STRING(FIRST)[LONG(SECOND)] = CHAR(THIRD);
  return FIRST;
}

DEFUN1(number_to_string_proc) {
  char buffer[100];
  if(is_fixnum(FIRST)) {
    snprintf(buffer, 100, "%ld", LONG(FIRST));
  } else if(is_real(FIRST)) {
    snprintf(buffer, 100, "%lf", DOUBLE(FIRST));
  } else {
    throw_interp("obj is not a number");
  }
  return make_string(buffer);
}

DEFUN1(string_to_number_proc) {
  return string_to_number(STRING(FIRST));
}

DEFUN1(symbol_to_string_proc) {
  return make_string(FIRST->data.symbol.value);
}

DEFUN1(string_to_symbol_proc) {
  return make_symbol(STRING(FIRST));
}

DEFUN1(string_to_uninterned_symbol_proc) {
  return make_uninterned_symbol(STRING(FIRST));
}

DEFUN1(make_compiled_proc_proc) {
  object *env = THIRD;
  if(is_the_empty_list(env)) {
    env = environment;
  }

  return make_compiled_proc(FIRST, SECOND, env);
}

DEFUN1(compiled_bytecode_proc) {
  return BYTECODE(FIRST);
}

DEFUN1(compiled_environment_proc) {
  return CENV(FIRST);
}

DEFUN1(is_vector_proc) {
  return AS_BOOL(is_vector(FIRST));
}

DEFUN1(make_vector_proc) {
  object *obj = make_vector(SECOND, LONG(FIRST));
  return obj;
}

DEFUN1(get_vector_element_proc) {
  return VARRAY(FIRST)[LONG(SECOND)];
}

DEFUN1(set_vector_element_proc) {
  VARRAY(FIRST)[LONG(SECOND)] = THIRD;
  return THIRD;
}

DEFUN1(is_hashtab_proc) {
  return AS_BOOL(is_hashtab(FIRST));
}

DEFUN1(make_hashtab_proc) {
  return make_hashtab(LONG(FIRST));
}

DEFUN1(set_hashtab_proc) {
  set_hashtab(FIRST, SECOND, THIRD);
  return THIRD;
}

DEFUN1(get_hashtab_proc) {
  return get_hashtab(FIRST, SECOND, THIRD);
}

DEFUN1(get_hashtab_keys_proc) {
  return get_hashtab_keys(FIRST);
}

DEFUN1(remkey_hashtab_proc) {
  remkey_hashtab(FIRST, SECOND);
  return true;
}

DEFUN1(vector_length_proc) {
  return make_fixnum(VSIZE(FIRST));
}

DEFUN1(exit_proc) {
  eval_exit_hook(environment);
  exit((int)LONG(FIRST));
  return false;
}

DEFUN1(stats_proc) {
  object *temp = cons(make_fixnum(get_cons_count()), the_empty_list);
  push_root(&temp);
  temp = cons(make_symbol("cons"), temp);

  object *result = cons(temp, the_empty_list);
  push_root(&result);

  temp = cons(make_fixnum(get_alloc_count()), the_empty_list);
  temp = cons(make_symbol("alloc"), temp);
  result = cons(temp, result);

  pop_root(&result);
  pop_root(&temp);

  return result;
}

DEFUN1(mark_and_sweep_proc) {
  return make_fixnum(baker_collect());
}

DEFUN1(clock_proc) {
  return make_fixnum(clock());
}

DEFUN1(clocks_per_sec_proc) {
  return make_fixnum(CLOCKS_PER_SEC);
}

DEFUN1(concat_proc) {
  char buffer[100];

  char *str1 = STRING(FIRST);
  char *str2 = STRING(SECOND);
  snprintf(buffer, 100, "%s%s", str1, str2);
  return make_string(buffer);
}

DEFUN1(current_environment_proc) {
  return environment;
}

DEFUN1(compound_args_proc) {
  return FIRST->data.compound_proc.parameters;
}

DEFUN1(compound_body_proc) {
  return FIRST->data.compound_proc.body;
}

DEFUN1(compound_env_proc) {
  return FIRST->data.compound_proc.env;
}

DEFUN1(is_meta_proc) {
  return AS_BOOL(is_meta(FIRST));
}

DEFUN1(meta_wrap_proc) {
  return make_meta_proc(FIRST, SECOND);
}

DEFUN1(get_meta_data_proc) {
  return METADATA(FIRST);
}

DEFUN1(get_meta_obj_proc) {
  return METAPROC(FIRST);
}

void write_pair(FILE * out, object * pair) {
  object *car_obj = car(pair);
  object *cdr_obj = cdr(pair);

  owrite(out, car_obj);
  if(is_pair(cdr_obj)) {
    fprintf(out, " ");
    write_pair(out, cdr_obj);
  }
  else if(is_the_empty_list(cdr_obj)) {
    return;
  }
  else {
    fprintf(out, " . ");
    owrite(out, cdr_obj);
  }
}

void owrite(FILE * out, object * obj) {
  long ii;
  char c;
  char *str;
  object *head;

  if(obj == NULL) {
    fprintf(out, "#<NULL>");
    return;
  }

  if(obj == the_global_environment) {
    fprintf(out, "#<global-environment>");
    return;
  }

  switch (obj->type) {
  case THE_EMPTY_LIST:
    fprintf(out, "()");
    break;
  case BOOLEAN:
    fprintf(out, "#%c", is_false(obj) ? 'f' : 't');
    break;
  case SYMBOL:
    fprintf(out, "%s", obj->data.symbol.value);
    break;
  case FIXNUM:
    fprintf(out, "%ld", LONG(obj));
    break;
  case FLOATNUM:
    fprintf(out, "%lf", DOUBLE(obj));
    break;
  case CHARACTER:
    fprintf(out, "#\\");
    c = obj->data.character.value;
    switch (c) {
    case '\n':
      fprintf(out, "newline");
      break;
    case ' ':
      fprintf(out, "space");
      break;
    default:
      putc(c, out);
    }
    break;
  case STRING:
    str = obj->data.string.value;
    putc('"', out);
    while(*str != '\0') {
      switch (*str) {
      case '\n':
	fprintf(out, "\\n");
	break;
      case '\\':
	fprintf(out, "\\\\");
	break;
      case '"':
	fprintf(out, "\\\"");
	break;
      default:
	putc(*str, out);
      }
      str++;
    }
    putc('"', out);
    break;
  case VECTOR:
    putc('#', out);
    putc('(', out);
    for(ii = 0; ii < VSIZE(obj); ++ii) {
      if(ii > 0) {
	putc(' ', out);
      }
      owrite(out, VARRAY(obj)[ii]);
    }
    putc(')', out);
    break;
  case PAIR:
    head = car(obj);
    /* only the reader produces these and the reader always
     * makes them like (thing affected-thing) therefore it
     * should be safe to assume that if obj's head is one of
     * these things then that obj has a cadr
     */
    if(head == quote_symbol) {
      fprintf(out, "'");
      owrite(out, cadr(obj));
    }
    else if(head == unquote_symbol) {
      fprintf(out, ",");
      owrite(out, cadr(obj));
    }
    else if(head == unquotesplicing_symbol) {
      fprintf(out, ",@");
      owrite(out, cadr(obj));
    }
    else if(head == quasiquote_symbol) {
      fprintf(out, "`");
      owrite(out, cadr(obj));
    }
    else {
      fprintf(out, "(");
      write_pair(out, obj);
      fprintf(out, ")");
    }
    break;
  case PRIMITIVE_PROC:
    fprintf(out, "#<primitive-procedure>");
    break;
  case COMPOUND_PROC:
    fprintf(out, "#<compound-procedure>");
    break;
  case COMPILED_PROC:
    fprintf(out, "#<compiled-procedure>");
    break;
  case SYNTAX_PROC:
    fprintf(out, "#<syntax-procedure>");
    break;
  case META_PROC:
    fprintf(out, "#<meta: ");
    owrite(out, METAPROC(obj));
    fprintf(out, ">");
    break;
  case HASH_TABLE:
    fprintf(out, "#<hash-table>");
    break;
  case INPUT_PORT:
    fprintf(out, "#<input-port>");
    break;
  case OUTPUT_PORT:
    fprintf(out, "#<output-port>");
    break;
  case EOF_OBJECT:
    fprintf(out, "#<eof>");
    break;
  case ALIEN:
    fprintf(out, "#<alien-object %llX>", (unsigned long long)ALIEN_PTR(obj));
    break;
  default:
    throw_interp("cannot write unknown type: %d\n", obj->type);
  }
}

object *debug_write(char *msg, object * obj, int level) {
  if(debug_enabled) {
    int ii;
    for(ii = 0; ii < level; ++ii) {
      fprintf(stderr, " ");
    }

    fprintf(stderr, "%s: ", msg);
    owrite(stderr, obj);
    fprintf(stderr, "\n");
  }
  return obj;
}

char is_falselike(object * obj) {
  return obj == false || is_the_empty_list(obj);
}

#ifdef NODEBUG
#define DN(msg, obj, level, n) obj
#else
#define DN(msg, obj, level, n)			\
  (DEBUG_LEVEL >= n ? debug_write(msg, obj, level) : obj);
#endif

#define D1(msg, obj, level) DN(msg, obj, level, 1);
#define D2(msg, obj, level) DN(msg, obj, level, 2);

object *interp(object * exp, object * env) {
  push_root(&exp);
  push_root(&env);
  object *result = interp1(exp, env, 0);
  pop_root(&env);
  pop_root(&exp);
  return result;
}

object *expand_macro(object * macro, object * args, object * env, int level) {
  object *new_env = extend_environment(macro->data.compound_proc.parameters,
				       args,
				       env);
  push_root(&new_env);
  object *expanded = interp1(macro->data.compound_proc.body,
			     new_env, level);
  pop_root(&new_env);

  return expanded;
}

#ifdef INTERP_CALLSTACK
void push_callstack(object * target) {
  set_car(the_call_stack, cons(target, car(the_call_stack)));
}

void pop_callstack() {
  set_car(the_call_stack, cdr(car(the_call_stack)));
}
#else
#define push_callstack(o)
#define pop_callstack(o)
#endif

/* convenient tool for unbinding the arguments that must be bound
 * during interp1. We rebind to temp to force the evalation of result
 * if it's an exp
 */
#define INTERP_RETURN(result)			\
  do {						\
    object *temp = result;			\
    D1("result", temp, level);			\
    if(env_protected) {				\
      pop_root(&env);				\
    }						\
    pop_callstack();				\
    return temp;				\
  } while(0)


object *interp1(object * exp, object * env, int level) {
  /* we break the usual convention of assuming our own arguments are
   * protected here because the tail recursive call can rebind these
   * two items to something new
   */
  char env_protected = 0;
  push_callstack(exp);

interp_restart:
  D1("interpreting", exp, level);

  if(is_symbol(exp)) {
    INTERP_RETURN(lookup_variable_value(exp, env));
  }
  else if(is_atom(exp)) {
    INTERP_RETURN(exp);
  }
  else {
    object *head = car(exp);
    if(head == quote_symbol) {
      INTERP_RETURN(second(exp));
    }
    else if(head == begin_symbol) {
      exp = cdr(exp);
      if(is_the_empty_list(exp)) {
	throw_interp("begin must be followed by exp");
	INTERP_RETURN(NULL);
      }

      while(!is_the_empty_list(cdr(exp))) {
	interp1(car(exp), env, level + 1);
	exp = cdr(exp);
      }

      exp = car(exp);
      goto interp_restart;
    }
    else if(head == set_symbol) {
      object *args = cdr(exp);
      object *val = interp1(second(args), env, level + 1);
      push_root(&val);

      define_variable(first(args), val, env);

      pop_root(&val);

      INTERP_RETURN(val);
    }
    else if(head == if_symbol) {
      object *args = cdr(exp);
      object *predicate = interp1(first(args), env, level + 1);

      if(is_falselike(predicate)) {
	/* else is optional, if none return #f */
	if(is_the_empty_list(cdr(cdr(args)))) {
	  INTERP_RETURN(false);
	}
	else {
	  exp = third(args);
	}
      }
      else {
	exp = second(args);
      }
      goto interp_restart;
    }
    else if(head == lambda_symbol) {
      object *args = cdr(exp);
      object *body = cons(begin_symbol, cdr(args));
      push_root(&body);

      object *proc = make_compound_proc(first(args),
					body,
					env);
      pop_root(&body);
      INTERP_RETURN(proc);
    }
    else if(head == macro_symbol) {
      object *args = cdr(exp);
      object *body = cons(begin_symbol, cdr(args));
      push_root(&body);

      object *result = make_syntax_proc(first(args),
					body);
      pop_root(&body);
      INTERP_RETURN(result);
    }
    else {
      /* procedure application */
      object *fn = interp1(head, env, level + 1);
      push_root(&fn);

      object *args = cdr(exp);

      /* unwrap meta */
      if(is_meta(fn)) {
	fn = METAPROC(fn);
      }

      if(is_syntax_proc(fn)) {
	/* expand the macro and evaluate that */
	object *expansion = expand_macro(fn, args, env, level);
	if(is_pair(expansion)) {
	  /* replace the macro call with the result */
	  set_car(exp, car(expansion));
	  set_cdr(exp, cdr(expansion));
	}
	else {
	  exp = expansion;
	}
	pop_root(&fn);

	push_root(&exp);
	object *result = interp1(exp, env, level + 1);
	pop_root(&exp);
	INTERP_RETURN(result);
      }

      /* evaluate the arguments */
      object *evald_args = the_empty_list;
      object *result = the_empty_list;
      object *last = the_empty_list;
      push_root(&evald_args);
      push_root(&result);

      while(!is_the_empty_list(args)) {
	result = interp1(first(args), env, level + 1);

	if(evald_args == the_empty_list) {
	  evald_args = cons(result, the_empty_list);
	  last = evald_args;
	}
	else {
	  set_cdr(last, cons(result, the_empty_list));
	  last = cdr(last);
	}
	args = cdr(args);
      }

      /* dispatch the call */
      if(is_primitive_proc(fn)) {
	result = fn->data.primitive_proc.fn(evald_args, env);
	pop_root(&result);
	pop_root(&evald_args);
	pop_root(&fn);
	INTERP_RETURN(result);
      }
      else if(is_compound_proc(fn)) {
	env = extend_environment(fn->data.compound_proc.parameters,
				 evald_args, fn->data.compound_proc.env);
	if(!env_protected) {
	  push_root(&env);
	  env_protected = 1;
	}

	exp = fn->data.compound_proc.body;

	pop_root(&result);
	pop_root(&evald_args);
	pop_root(&fn);
	goto interp_restart;
      }
      else if(is_compiled_proc(fn)) {
	/* need to reverse the arguments */
	object *stack = make_vector(the_empty_list, 30);
	long stack_top = 0;

	push_root(&stack);

	while(!is_the_empty_list(evald_args)) {
	  VARRAY(stack)[stack_top++] = car(evald_args);
	  evald_args = cdr(evald_args);
	}

	result = vm_execute(fn, stack, stack_top);
	pop_root(&stack);

	pop_root(&result);
	pop_root(&evald_args);
	pop_root(&fn);
	INTERP_RETURN(result);
      }
      else {
	pop_root(&result);
	pop_root(&evald_args);
	pop_root(&fn);

	owrite(stderr, fn);
	throw_interp("\ncannot apply non-function\n");
	INTERP_RETURN(NULL);
      }
    }
  }

  owrite(stderr, exp);
  throw_interp(": can't evaluate\n");
  INTERP_RETURN(NULL);
}


void init_prim_environment(object * env) {
  /* used throughout this method to protect the thing in definition
   * from gc */
  object *curr = the_empty_list;
  push_root(&curr);

#define add_procedure(scheme_name, c_name)    \
  define_variable(make_symbol(scheme_name),   \
                  curr=make_primitive_proc(c_name),	\
                  env);

  add_procedure("null?", is_null_proc);
  add_procedure("boolean?", is_boolean_proc);
  add_procedure("symbol?", is_symbol_proc);
  add_procedure("integer?", is_integer_proc);
  add_procedure("real?", is_real_proc);
  add_procedure("char?", is_char_proc);
  add_procedure("string?", is_string_proc);
  add_procedure("pair?", is_pair_proc);
  add_procedure("procedure?", is_procedure_proc);
  add_procedure("compound-procedure?", is_compound_proc_proc);
  add_procedure("syntax-procedure?", is_syntax_proc_proc);
  add_procedure("output-port?", is_output_port_proc);
  add_procedure("input-port?", is_input_port_proc);
  add_procedure("eof-object?", is_eof_proc);
  add_procedure("alien?", is_alien_proc);
  add_procedure("compiled-procedure?", is_compiled_proc_proc);
  add_procedure("meta?", is_meta_proc);

  add_procedure("fixnum-add", add_fixnum_proc);
  add_procedure("real-add", add_real_proc);
  add_procedure("fixnum-sub", sub_fixnum_proc);
  add_procedure("real-sub", sub_real_proc);
  add_procedure("fixnum-mul", mul_fixnum_proc);
  add_procedure("real-mul", mul_real_proc);
  add_procedure("fixnum-div", div_fixnum_proc);
  add_procedure("real-div", div_real_proc);
  add_procedure("fixnum-mod", mod_fixnum_proc);
  add_procedure("real-mod", mod_real_proc);
  add_procedure("fixnum-pow", pow_fixnum_proc);
  add_procedure("real-pow", pow_real_proc);
  add_procedure("logand", logand_proc);
  add_procedure("logor", logor_proc);
  add_procedure("logxor", logxor_proc);
  add_procedure("ash", ash_proc);
  add_procedure("fixnum-sqrt", sqrt_fixnum_proc);
  add_procedure("real-sqrt", sqrt_real_proc);
  add_procedure("fixnum-log", log_fixnum_proc);
  add_procedure("real-log", log_real_proc);
  add_procedure("floor", floor_proc);
  add_procedure("ceiling", ceil_proc);
  add_procedure("round", round_proc);
  add_procedure("integer->real", fixnum_to_real_proc);
  add_procedure("fixnum-less-than", is_less_than_fixnum_proc);
  add_procedure("real-less-than", is_less_than_real_proc);
  add_procedure("fixnum-greater-than", is_greater_than_fixnum_proc);
  add_procedure("real-greater-than", is_greater_than_real_proc);
  add_procedure("fixnum-equal", is_number_equal_fixnum_proc);
  add_procedure("real-equal", is_number_equal_real_proc);

  add_procedure("cons", cons_proc);
  add_procedure("car", car_proc);
  add_procedure("cdr", cdr_proc);
  add_procedure("set-car!", set_car_proc);
  add_procedure("set-cdr!", set_cdr_proc);
  add_procedure("list", list_proc);
  add_procedure("vector?", is_vector_proc);
  add_procedure("make-vector", make_vector_proc);
  add_procedure("vector-length", vector_length_proc);
  add_procedure("vector-ref", get_vector_element_proc);
  add_procedure("vector-set!", set_vector_element_proc);
  add_procedure("make-hashtab-eq", make_hashtab_proc);
  add_procedure("hashtab?", is_hashtab_proc);
  add_procedure("hashtab-set!", set_hashtab_proc);
  add_procedure("hashtab-ref", get_hashtab_proc);
  add_procedure("hashtab-remove!", remkey_hashtab_proc);
  add_procedure("hashtab-keys", get_hashtab_keys_proc);
  add_procedure("meta-wrap", meta_wrap_proc);
  add_procedure("meta-object", get_meta_obj_proc);
  add_procedure("meta-data", get_meta_data_proc);

  add_procedure("eq?", is_eq_proc);

  add_procedure("open-output-port", open_output_port_proc);
  add_procedure("open-input-port", open_input_port_proc);
  add_procedure("close-output-port", close_output_port_proc);
  add_procedure("close-input-port", close_input_port_proc);

  add_procedure("write-port", write_proc);
  add_procedure("read-port", read_proc);
  add_procedure("read-char", read_char_proc);
  add_procedure("write-char", write_char_proc);
  add_procedure("unread-char", unread_char_proc);

  add_procedure("eval", eval_proc);
  add_procedure("apply", apply_proc);

  add_procedure("char->integer", char_to_integer_proc);
  add_procedure("integer->char", integer_to_char_proc);
  add_procedure("make-string", make_string_proc);
  add_procedure("string-ref", string_ref_proc);
  add_procedure("string-set!", string_set_proc);
  add_procedure("number->string", number_to_string_proc);
  add_procedure("string->number", string_to_number_proc);
  add_procedure("symbol->string", symbol_to_string_proc);
  add_procedure("string->symbol", string_to_symbol_proc);
  add_procedure("string->uninterned-symbol",
		string_to_uninterned_symbol_proc);

  add_procedure("prim-concat", concat_proc);

  add_procedure("exit", exit_proc);
  add_procedure("interpreter-stats", stats_proc);
  add_procedure("mark-and-sweep", mark_and_sweep_proc);
  add_procedure("clock", clock_proc);
  add_procedure("clocks-per-sec", clocks_per_sec_proc);
  add_procedure("set-debug!", debug_proc);
  add_procedure("current-environment", current_environment_proc);
  add_procedure("compound-body", compound_body_proc);
  add_procedure("compound-args", compound_args_proc);
  add_procedure("compound-environment", compound_env_proc);

  add_procedure("make-compiled-proc", make_compiled_proc_proc);
  add_procedure("compiled-bytecode", compiled_bytecode_proc);
  add_procedure("compiled-environment", compiled_environment_proc);

  define_variable(stdin_symbol, curr = make_input_port(stdin), env);
  define_variable(stdout_symbol, curr = make_output_port(stdout), env);
  define_variable(stderr_symbol, curr = make_output_port(stderr), env);
  define_variable(make_symbol("callstack"), the_call_stack, env);
  define_variable(make_symbol("base-env"), env, env);
  define_variable(exit_hook_symbol, the_empty_list, env);

  pop_root(&curr);
}

void init() {
  gc_init();

  the_empty_list = alloc_object(0);
  the_empty_list->type = THE_EMPTY_LIST;
  the_empty_list->data.pair.car = the_empty_list;
  the_empty_list->data.pair.cdr = the_empty_list;
  push_root(&the_empty_list);

  the_empty_vector = alloc_object(0);
  the_empty_vector->type = VECTOR;
  VSIZE(the_empty_vector) = 0;
  push_root(&the_empty_vector);

  false = alloc_object(0);
  false->type = BOOLEAN;
  false->data.boolean.value = 0;
  push_root(&false);

  true = alloc_object(0);
  true->type = BOOLEAN;
  true->data.boolean.value = 1;
  push_root(&true);

  symbol_table = the_empty_list;
  push_root(&symbol_table);

  unquote_symbol = make_symbol("unquote");
  unquotesplicing_symbol = make_symbol("unquotesplicing");
  quote_symbol = make_symbol("quote");
  quasiquote_symbol = make_symbol("quasiquote");
  set_symbol = make_symbol("set!");
  if_symbol = make_symbol("if");
  begin_symbol = make_symbol("begin");
  lambda_symbol = make_symbol("lambda");
  macro_symbol = make_symbol("macro");
  stdin_symbol = make_symbol("stdin");
  stdout_symbol = make_symbol("stdout");
  stderr_symbol = make_symbol("stderr");
  exit_hook_symbol = make_symbol("exit-hook");

  eof_object = alloc_object(0);
  eof_object->type = EOF_OBJECT;
  push_root(&eof_object);

  the_empty_environment = the_empty_list;
  the_global_environment = setup_environment();
  push_root(&the_global_environment);

  the_call_stack = cons(the_empty_list, the_empty_list);
  push_root(&the_call_stack);

  vm_init();
  init_prim_environment(the_global_environment);
}

/**
 * handy for user side debugging */
void print_obj(object * obj) {
  owrite(stdout, obj);
  printf("\n");
}
