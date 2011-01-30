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
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>
#include <sys/stat.h>

#include "types.h"
#include "interp.h"
#include "read.h"
#include "gc.h"
#include "vm.h"
#include "ffi.h"

static const int DEBUG_LEVEL = 1;

object *throw_interp(char *msg, ...) {
  char buffer[1024];
  va_list args;
  va_start(args, msg);
  vsnprintf(buffer, 1024, msg, args);
  va_end(args);

  object *msg_obj = make_string(buffer);
  push_root(&msg_obj);
  object *ex = make_primitive_exception(msg_obj);
  pop_root(&msg_obj);

  return ex;
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

object *lookup_global_value(object * var, object * env) {
  object *res = get_hashtab(env, var, NULL);
  if(res == NULL) {
    return throw_interp("lookup failed. variable %s is unbound", STRING(var));
  }

  return res;
}

object *lookup_variable_value(object * var, object * env) {
  object *frame;
  object *vars;
  object *vals;

  while(!is_the_empty_list(env)) {
    frame = first_frame(env);
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
    env = enclosing_environment(env);
  }

  /* check the global environment */
  return lookup_global_value(var, g->env);
}

void define_global_variable(object * var, object * new_val, object * env) {
  set_hashtab(env, var, new_val);
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
    senv = enclosing_environment(senv);
  }

  /* we define at the global level */
  define_global_variable(var, new_val, g->env);
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

DEFUN1(tag_macro_proc) {
  FIRST->type = SYNTAX_PROC;
  return FIRST;
}

DEFUN1(is_procedure_proc) {
  object *obj = FIRST;

  /* unwrap meta */
  if(is_meta(obj)) {
    obj = METAPROC(obj);
  }

  return AS_BOOL(is_primitive_proc(obj) || is_compound_proc(obj) ||
		 is_compiled_proc(obj));
}

DEFUN1(is_compound_proc_proc) {
  return AS_BOOL(is_compound_proc(FIRST));
}

DEFUN1(is_syntax_proc_proc) {
  return AS_BOOL(is_syntax_proc(FIRST)
		 || is_compiled_syntax_proc(FIRST));
}

DEFUN1(is_compiled_syntax_proc_proc) {
  return AS_BOOL(is_compiled_syntax_proc(FIRST));
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
  return make_fixnum(LONG(FIRST) + LONG(SECOND));
}

DEFUN1(add_real_proc) {
  return make_real(DOUBLE(FIRST) + DOUBLE(SECOND));
}

DEFUN1(sub_fixnum_proc) {
  return make_fixnum(LONG(FIRST) - LONG(SECOND));
}

DEFUN1(sub_real_proc) {
  return make_real(DOUBLE(FIRST) - DOUBLE(SECOND));
}

DEFUN1(mul_fixnum_proc) {
  return make_fixnum(LONG(FIRST) * LONG(SECOND));
}

DEFUN1(mul_real_proc) {
  return make_real(DOUBLE(FIRST) * DOUBLE(SECOND));
}

DEFUN1(div_fixnum_proc) {
  return make_fixnum(LONG(FIRST) / LONG(SECOND));
}

DEFUN1(div_real_proc) {
  return make_real(DOUBLE(FIRST) / DOUBLE(SECOND));
}

DEFUN1(mod_fixnum_proc) {
  return make_fixnum(LONG(FIRST) % LONG(SECOND));
}

DEFUN1(mod_real_proc) {
  return make_real(fmod(DOUBLE(FIRST), DOUBLE(SECOND)));
}

DEFUN1(pow_fixnum_proc) {
  return make_fixnum(pow(LONG(FIRST), LONG(SECOND)));
}

DEFUN1(pow_real_proc) {
  return make_real(pow(DOUBLE(FIRST), DOUBLE(SECOND)));
}

DEFUN1(logand_proc) {
  return make_fixnum(LONG(FIRST) & LONG(SECOND));
}

DEFUN1(logor_proc) {
  return make_fixnum(LONG(FIRST) | LONG(SECOND));
}

DEFUN1(logxor_proc) {
  return make_fixnum(LONG(FIRST) ^ LONG(SECOND));
}

DEFUN1(ash_proc) {
  unsigned long result = LONG(FIRST);
  long n = LONG(SECOND);

  if(n > 0)
    result <<= n;
  else
    result >>= -n;

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

double round(double);

DEFUN1(round_proc) {
  return make_fixnum((long)round(DOUBLE(FIRST)));
}

DEFUN1(fixnum_to_real_proc) {
  return make_real((double)LONG(FIRST));
}

DEFUN1(debug_proc) {
  if(FIRST == g->false) {
    g->debug_enabled = 0;
  }
  else {
    g->debug_enabled = 1;
  }
  return FIRST;
}

DEFUN1(cons_proc) {
  return cons(FIRST, SECOND);
}

DEFUN1(car_proc) {
  object *first = FIRST;
  if(!is_pair(first) && !is_the_empty_list(first))
    return throw_interp("car expects list");
  return car(first);
}

DEFUN1(cdr_proc) {
  object *first = FIRST;
  if(!is_pair(first) && !is_the_empty_list(first))
    return throw_interp("cdr expects list");
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

DEFUN1(is_eq_proc) {
  if(FIRST->type != SECOND->type) {
    return g->false;
  }
  switch (FIRST->type) {
  case FIXNUM:
    return (LONG(FIRST) == LONG(SECOND)) ? g->true : g->false;
  case FLOATNUM:
    return (DOUBLE(FIRST) == DOUBLE(SECOND)) ? g->true : g->false;
  case CHARACTER:
    return (CHAR(FIRST) == CHAR(SECOND)) ? g->true : g->false;
  case STRING:
    return (strcmp(STRING(FIRST), STRING(SECOND)) == 0) ? g->true : g->false;
  default:
    return (FIRST == SECOND) ? g->true : g->false;
  }
}

DEFUN1(is_number_equal_fixnum_proc) {
  return AS_BOOL(LONG(FIRST) == LONG(SECOND));
}

DEFUN1(is_number_equal_real_proc) {
  return AS_BOOL(DOUBLE(FIRST) == DOUBLE(SECOND));
}

DEFUN1(is_less_than_fixnum_proc) {
  return AS_BOOL(LONG(FIRST) < LONG(SECOND));
}

DEFUN1(is_less_than_real_proc) {
  return AS_BOOL(DOUBLE(FIRST) < DOUBLE(SECOND));
}

DEFUN1(is_greater_than_fixnum_proc) {
  return AS_BOOL(LONG(FIRST) > LONG(SECOND));
}

DEFUN1(is_greater_than_real_proc) {
  return AS_BOOL(DOUBLE(FIRST) > DOUBLE(SECOND));
}

DEFUN1(open_output_port_proc) {
  object *name = FIRST;
  FILE *out = fopen(STRING(name), "w");
  if(out == NULL) {
    return g->eof_object;
  }
  return make_output_port(out);
}

DEFUN1(close_output_port_proc) {
  FILE *out = OUTPUT(FIRST);
  fclose(out);
  return g->true;
}

DEFUN1(open_input_port_proc) {
  object *name = FIRST;
  FILE *in = fopen(STRING(name), "r");
  if(in == NULL) {
    return g->eof_object;
  }
  return make_input_port(in);
}

DEFUN1(close_input_port_proc) {
  FILE *in = INPUT(FIRST);
  fclose(in);
  return g->true;
}

DEFUN1(chmod_proc) {
  int r = chmod(STRING(FIRST), LONG(SECOND));
  if (r == 0)
    return g->true;
  return g->false;
}

DEFUN1(umask_proc) {
  return make_fixnum(umask(LONG(FIRST)));
}

DEFUN1(mkdir_proc) {
  int r = mkdir(STRING(FIRST), LONG(SECOND));
  if (r == 0)
    return g->true;
  return g->false;

}

/* getumask has a thread-safety issue. */
DEFUN1(getumask_proc) {
  mode_t mode = umask(0);
  umask(mode);
  return make_fixnum(mode);
}

DEFUN1(rename_proc) {
  int r = rename(STRING(FIRST), STRING(SECOND));
  if (r == 0)
    return g->true;
  return g->false;
}

DEFUN1(gc_proc) {
  return make_fixnum(baker_collect());
}

DEFUN1(eval_proc) {
  object *exp = FIRST;
  return interp(exp, g->empty_env);
}

DEFUN1(save_image_proc) {
  baker_collect();

  object *file = FIRST;
  int r = save_image(STRING(file));
  if (r < 0)
    return throw_interp("could not save image");
  return g->true;
}

object *apply(object *fn, object *evald_args) {
  /* essentially duplicated from interp but I'm not
   * sure how to implement this properly otherwise.*/
  object *env;
  object *exp;
  object *result;

  /* unwrap meta */
  if(is_meta(fn)) {
    fn = METAPROC(fn);
  }

  if(is_primitive_proc(fn) || is_compiled_proc(fn) ||
     is_compiled_syntax_proc(fn)) {

    object *stack = make_vector(g->empty_list, 30);
    long stack_top = 0;

    push_root(&stack);

    long num_args = 0;
    while(!is_the_empty_list(evald_args)) {
      VPUSH(car(evald_args), stack, stack_top);
      ++num_args;
      evald_args = cdr(evald_args);
    }

    if(is_primitive_proc(fn)) {
      result = fn->data.primitive_proc.fn(stack, num_args, stack_top);
      /* no need to unwind the stack since it's just going to be
         gc'd */
    }
    else {
      result = vm_execute(fn, stack, stack_top, num_args);
    }
    pop_root(&stack);
    return result;
  }
  else if(is_compound_proc(fn) || is_syntax_proc(fn)) {
    env = extend_environment(COMPOUND_PARAMS(fn),
			     evald_args, COMPOUND_ENV(fn));
    push_root(&env);
    exp = COMPOUND_BODY(fn);
    result = interp(exp, env);
    pop_root(&env);
    return result;
  }

  owrite(stderr, fn);
  return throw_interp("cannot apply non-function");
}

DEFUN1(apply_proc) {
  object *fn = FIRST;
  object *evald_args = SECOND;
  return apply(fn, evald_args);
}

object *obj_read(FILE * in);
DEFUN1(read_proc) {
  object *in_port = FIRST;
  object *result = obj_read(INPUT(in_port));
  return (result == NULL) ? g->eof_object : result;
}

DEFUN1(write_proc) {
  object *port = SECOND;
  object *obj = FIRST;
  owrite(OUTPUT(port), obj);
  return g->true;
}

DEFUN1(write_char_proc) {
  object *port = SECOND;
  object *ch = FIRST;
  putc(CHAR(ch), OUTPUT(port));
  return g->true;
}

DEFUN1(read_char_proc) {
  object *port = FIRST;
  int result = getc(INPUT(port));
  return (result == EOF) ? g->eof_object : make_character(result);
}

DEFUN1(unread_char_proc) {
  object *port = FIRST;
  object *ch = SECOND;

  ungetc(CHAR(ch), INPUT(port));
  return g->true;
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

int snprintf(char *, size_t, const char *, ...);

DEFUN1(number_to_string_proc) {
  char buffer[100];
  if(is_fixnum(FIRST)) {
    snprintf(buffer, 100, "%ld", LONG(FIRST));
  }
  else if(is_real(FIRST)) {
    snprintf(buffer, 100, "%.15lg", DOUBLE(FIRST));
  }
  else {
    return throw_interp("obj is not a number");
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
  return make_compiled_proc(FIRST, SECOND);
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
  return g->true;
}

DEFUN1(vector_length_proc) {
  return make_fixnum(VSIZE(FIRST));
}

DEFUN1(exit_proc) {
  exit((int)LONG(FIRST));
  return g->false;
}

DEFUN1(stats_proc) {
  object *temp = cons(make_fixnum(get_cons_count()), g->empty_list);
  push_root(&temp);
  temp = cons(make_symbol("cons"), temp);

  object *result = cons(temp, g->empty_list);
  push_root(&result);

  temp = cons(make_fixnum(get_alloc_count()), g->empty_list);
  temp = cons(make_symbol("alloc"), temp);
  result = cons(temp, result);

  pop_root(&result);
  pop_root(&temp);

  return result;
}

DEFUN1(clock_proc) {
  return make_fixnum(clock());
}

DEFUN1(getpid_proc) {
  return make_fixnum(getpid());
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

DEFUN1(compound_args_proc) {
  return COMPOUND_PARAMS(FIRST);
}

DEFUN1(compound_body_proc) {
  return COMPOUND_BODY(FIRST);
}

DEFUN1(compound_env_proc) {
  return COMPOUND_ENV(FIRST);
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

object *write_pair(FILE * out, object * pair) {
  object *car_obj = car(pair);
  object *cdr_obj = cdr(pair);

  object * result = owrite(out, car_obj);
  if(is_primitive_exception(result)) {
    return result;
  }

  if(is_pair(cdr_obj)) {
    fprintf(out, " ");
    return write_pair(out, cdr_obj);
  }
  else if(is_the_empty_list(cdr_obj)) {
    return g->true;
  }
  else {
    fprintf(out, " . ");
    return owrite(out, cdr_obj);
  }
}

object *owrite(FILE * out, object * obj) {
  long ii;
  char c;
  char *str;
  object *head;

  if(obj == NULL) {
    return throw_interp("object is primitive #<NULL>");
  }

  if(is_hashtab(obj) && obj == g->env) {
    fprintf(out, "#<global-environment-hashtab>");
    return g->true;
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
      object * result = owrite(out, VARRAY(obj)[ii]);
      if(is_primitive_exception(result)) {
	return result;
      }
    }
    putc(')', out);
    break;
  case PAIR:
    head = car(obj);
    /*
     * It's not actually safe to cadr these objects because the user
     * could have typed in something like (quote) which would cause us
     * to car on nil
     */
    if(head == g->quote_symbol) {
      fprintf(out, "'");
      object *result = owrite(out, cadr(obj));
      if(is_primitive_exception(result)) {
	return result;
      }
    }
    else if(head == g->unquote_symbol) {
      fprintf(out, ",");
      object *result = owrite(out, cadr(obj));
      if(is_primitive_exception(result)) {
	return result;
      }
    }
    else if(head == g->unquotesplicing_symbol) {
      fprintf(out, ",@");
      object *result = owrite(out, cadr(obj));
      if(is_primitive_exception(result)) {
	return result;
      }
    }
    else if(head == g->quasiquote_symbol) {
      fprintf(out, "`");
      object *result = owrite(out, cadr(obj));
      if(is_primitive_exception(result)) {
	return result;
      }
    }
    else {
      fprintf(out, "(");
      object *result = write_pair(out, obj);
      if(is_primitive_exception(result)) {
	return result;
      }
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
  case COMPILED_SYNTAX_PROC:
    fprintf(out, "#<compiled-syntax-procedure>");
    break;
  case SYNTAX_PROC:
    fprintf(out, "#<syntax-procedure>");
    break;
  case META_PROC:
  {
    fprintf(out, "#<meta: ");
    object *result = owrite(out, METAPROC(obj));
    if(is_primitive_exception(result)) {
      return result;
    }
    fprintf(out, ">");
    break;
  }
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
    return throw_interp("cannot write unknown type: %d\n", obj->type);
  }
  return g->true;
}

char is_falselike(object * obj) {
  return obj == g->false || is_the_empty_list(obj);
}

#define DN(msg, obj, level, n) obj

object *interp(object * exp, object * env) {
  push_root(&exp);
  push_root(&env);

  object *prim_call_stack = make_vector(g->empty_list, 10);
  long prim_stack_top = 0;

  push_root(&prim_call_stack);

  object *result = interp1(exp, env, 0, prim_call_stack, prim_stack_top);

  pop_root(&prim_call_stack);

  pop_root(&env);
  pop_root(&exp);
  return result;
}

object *expand_macro(object * macro, object * args, object * env, int level,
		     object * stack, long stack_top) {
  object *new_env = extend_environment(COMPOUND_PARAMS(macro),
				       args,
				       env);
  push_root(&new_env);
  object *expanded = interp1(COMPOUND_BODY(macro),
			     new_env, level, stack, stack_top);
  pop_root(&new_env);

  return expanded;
}

/* convenient tool for unbinding the arguments that must be bound
 * during interp1. We rebind to temp to force the evalation of result
 * if it's an exp
 */
#define INTERP_RETURN(result)			\
  do {						\
    object *temp = result;			\
    if(env_protected) {				\
      pop_root(&env);				\
    }						\
    return temp;				\
  } while(0)


object *interp1(object * exp, object * env, int level,
		object * prim_call_stack, long prim_stack_top) {
  /* we break the usual convention of assuming our own arguments are
   * protected here because the tail recursive call can rebind these
   * two items to something new
   */
  char env_protected = 0;

interp_restart:

  if(is_symbol(exp)) {
    INTERP_RETURN(lookup_variable_value(exp, env));
  }
  else if(is_atom(exp)) {
    INTERP_RETURN(exp);
  }
  else {
    object *head = car(exp);
    if(head == g->quote_symbol) {
      INTERP_RETURN(second(exp));
    }
    else if(head == g->begin_symbol) {
      exp = cdr(exp);
      if(is_the_empty_list(exp)) {
	INTERP_RETURN(throw_interp("begin must be followed by exp"));
      }

      while(!is_the_empty_list(cdr(exp))) {
	interp1(car(exp), env, level + 1, prim_call_stack, prim_stack_top);
	exp = cdr(exp);
      }

      exp = car(exp);
      goto interp_restart;
    }
    else if(head == g->set_symbol) {
      object *args = cdr(exp);
      object *val = interp1(second(args), env, level + 1, prim_call_stack,
			    prim_stack_top);
      push_root(&val);

      define_variable(first(args), val, env);

      pop_root(&val);

      INTERP_RETURN(val);
    }
    else if(head == g->if_symbol) {
      object *args = cdr(exp);
      object *predicate =
	interp1(first(args), env, level + 1, prim_call_stack, prim_stack_top);

      if(is_falselike(predicate)) {
	/* else is optional, if none return #f */
	if(is_the_empty_list(cdr(cdr(args)))) {
	  INTERP_RETURN(g->false);
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
    else if(head == g->lambda_symbol) {
      object *args = cdr(exp);
      object *body = cons(g->begin_symbol, cdr(args));
      push_root(&body);

      object *proc = make_compound_proc(first(args),
					body,
					env);
      pop_root(&body);
      INTERP_RETURN(proc);
    }
    else {
      /* procedure application */
      object *fn =
	interp1(head, env, level + 1, prim_call_stack, prim_stack_top);
      push_root(&fn);

      object *args = cdr(exp);

      /* unwrap meta */
      if(is_meta(fn)) {
	fn = METAPROC(fn);
      }

      if(is_syntax_proc(fn)) {
	/* expand the macro and evaluate that */
	object *expansion =
	  expand_macro(fn, args, env, level, prim_call_stack, prim_stack_top);
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
	object *result =
	  interp1(exp, env, level + 1, prim_call_stack, prim_stack_top);
	pop_root(&exp);
	INTERP_RETURN(result);
      }

      /* evaluate the arguments and dispatch the call */
      if(is_primitive_proc(fn) || is_compiled_proc(fn)
	 || is_compiled_syntax_proc(fn)) {
	long arg_count = 0;
	object *result;
	while(!is_the_empty_list(args)) {
	  result =
	    interp1(first(args), env, level + 1, prim_call_stack,
		    prim_stack_top);
	  VPUSH(result, prim_call_stack, prim_stack_top);
	  ++arg_count;
	  args = cdr(args);
	}

	if(is_primitive_proc(fn)) {
	  result =
	    fn->data.primitive_proc.fn(prim_call_stack, arg_count,
				       prim_stack_top);

	  /* clear out the stack since primitives will not */
	  long idx;
	  object *temp;
	  for(idx = 0; idx < arg_count; ++idx) {
	    VPOP(temp, prim_call_stack, prim_stack_top);
	  }
	}
	else {
	  result = vm_execute(fn, prim_call_stack, prim_stack_top, arg_count);
	}

	pop_root(&fn);
	INTERP_RETURN(result);
      }
      else if(is_compound_proc(fn)) {
	/* compounds take their arguments as a linked list */
	object *evald_args = g->empty_list;
	object *result = g->empty_list;
	object *last = g->empty_list;
	push_root(&evald_args);
	push_root(&result);

	while(!is_the_empty_list(args)) {
	  result =
	    interp1(first(args), env, level + 1, prim_call_stack,
		    prim_stack_top);

	  if(evald_args == g->empty_list) {
	    evald_args = cons(result, g->empty_list);
	    last = evald_args;
	  }
	  else {
	    set_cdr(last, cons(result, g->empty_list));
	    last = cdr(last);
	  }
	  args = cdr(args);
	}

	/* dispatch the call */
	env = extend_environment(COMPOUND_PARAMS(fn),
				 evald_args, COMPOUND_ENV(fn));
	if(!env_protected) {
	  push_root(&env);
	  env_protected = 1;
	}

	exp = COMPOUND_BODY(fn);

	pop_root(&result);
	pop_root(&evald_args);
	pop_root(&fn);
	goto interp_restart;
      }
      else {
	pop_root(&fn);

	owrite(stderr, fn);
	INTERP_RETURN(throw_interp("\ncannot apply non-function\n"));
      }
    }
  }

  owrite(stderr, exp);
  INTERP_RETURN(throw_interp(": can't evaluate\n"));
}


void interp_definer(char *sym, object *value) {
  push_root(&value);
  object * symbol = make_symbol(sym);
  define_global_variable(symbol, value, g->env);
  pop_root(&value);
}

void init_prim_environment(definer defn) {
  /* used throughout this method to protect the thing in definition
   * from gc */

#define add_procedure(scheme_name, c_name)			\
  defn(scheme_name,						\
       make_primitive_proc(c_name));


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
  add_procedure("compiled-syntax-procedure?", is_compiled_syntax_proc_proc);
  add_procedure("output-port?", is_output_port_proc);
  add_procedure("input-port?", is_input_port_proc);
  add_procedure("eof-object?", is_eof_proc);
  add_procedure("alien?", is_alien_proc);
  add_procedure("compiled-procedure?", is_compiled_proc_proc);
  add_procedure("meta?", is_meta_proc);

  add_procedure("set-macro!", tag_macro_proc);
  add_procedure("%fixnum-add", add_fixnum_proc);
  add_procedure("%real-add", add_real_proc);
  add_procedure("%fixnum-sub", sub_fixnum_proc);
  add_procedure("%real-sub", sub_real_proc);
  add_procedure("%fixnum-mul", mul_fixnum_proc);
  add_procedure("%real-mul", mul_real_proc);
  add_procedure("%fixnum-div", div_fixnum_proc);
  add_procedure("%real-div", div_real_proc);
  add_procedure("%fixnum-mod", mod_fixnum_proc);
  add_procedure("%real-mod", mod_real_proc);
  add_procedure("%fixnum-pow", pow_fixnum_proc);
  add_procedure("%real-pow", pow_real_proc);
  add_procedure("%logand", logand_proc);
  add_procedure("%logor", logor_proc);
  add_procedure("%logxor", logxor_proc);
  add_procedure("%ash", ash_proc);
  add_procedure("%fixnum-sqrt", sqrt_fixnum_proc);
  add_procedure("%real-sqrt", sqrt_real_proc);
  add_procedure("%fixnum-log", log_fixnum_proc);
  add_procedure("%real-log", log_real_proc);
  add_procedure("%floor", floor_proc);
  add_procedure("%ceiling", ceil_proc);
  add_procedure("%round", round_proc);
  add_procedure("%integer->real", fixnum_to_real_proc);
  add_procedure("%fixnum-less-than", is_less_than_fixnum_proc);
  add_procedure("%real-less-than", is_less_than_real_proc);
  add_procedure("%fixnum-greater-than", is_greater_than_fixnum_proc);
  add_procedure("%real-greater-than", is_greater_than_real_proc);
  add_procedure("%fixnum-equal", is_number_equal_fixnum_proc);
  add_procedure("%real-equal", is_number_equal_real_proc);

  add_procedure("cons", cons_proc);
  add_procedure("car", car_proc);
  add_procedure("cdr", cdr_proc);
  add_procedure("%set-car!", set_car_proc);
  add_procedure("%set-cdr!", set_cdr_proc);
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
  add_procedure("%chmod", chmod_proc);
  add_procedure("%umask", umask_proc);
  add_procedure("getumask", getumask_proc);
  add_procedure("%mkdir", mkdir_proc);
  add_procedure("%rename-file", rename_proc);

  add_procedure("write-port", write_proc);
  add_procedure("read-port", read_proc);
  add_procedure("read-char", read_char_proc);
  add_procedure("write-char", write_char_proc);
  add_procedure("unread-char", unread_char_proc);

  add_procedure("eval", eval_proc);
  add_procedure("apply", apply_proc);
  add_procedure("gc", gc_proc);
  add_procedure("%save-image", save_image_proc);

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
  add_procedure("clock", clock_proc);
  add_procedure("getpid", getpid_proc);
  add_procedure("clocks-per-sec", clocks_per_sec_proc);
  add_procedure("set-debug!", debug_proc);
  add_procedure("compound-body", compound_body_proc);
  add_procedure("compound-args", compound_args_proc);
  add_procedure("compound-environment", compound_env_proc);

  add_procedure("make-compiled-proc", make_compiled_proc_proc);
  add_procedure("compiled-bytecode", compiled_bytecode_proc);
  add_procedure("compiled-environment", compiled_environment_proc);

  defn(SYMBOL(g->stdin_symbol), make_input_port(stdin));
  defn(SYMBOL(g->stdout_symbol), make_output_port(stdout));
  defn(SYMBOL(g->stderr_symbol), make_output_port(stderr));
  defn(SYMBOL(g->exit_hook_symbol), g->empty_list);
}

void interp_add_roots(void) {
  push_root(&(g->empty_list));
  push_root(&(g->empty_vector));
  push_root(&(g->true));
  push_root(&(g->symbol_table));
  push_root(&(g->eof_object));
  push_root(&(g->env));
  push_root(&(g->vm_env));
  push_root(&(g->error_sym));
}

void init() {
  gc_init();

  g->debug_enabled = 0;

  g->empty_list = alloc_object(0);
  g->empty_list->type = THE_EMPTY_LIST;
  g->empty_list->data.pair.car = g->empty_list;
  g->empty_list->data.pair.cdr = g->empty_list;
  push_root(&(g->empty_list));

  g->empty_vector = alloc_object(0);
  g->empty_vector->type = VECTOR;
  VSIZE(g->empty_vector) = 0;
  push_root(&(g->empty_vector));

  g->false = alloc_object(0);
  g->false->type = BOOLEAN;
  g->false->data.boolean.value = 0;
  push_root(&(g->false));

  g->true = alloc_object(0);
  g->true->type = BOOLEAN;
  g->true->data.boolean.value = 1;
  push_root(&(g->true));

  g->symbol_table = g->empty_list;
  push_root(&(g->symbol_table));

  g->unquote_symbol = make_symbol("unquote");
  g->unquotesplicing_symbol = make_symbol("unquotesplicing");
  g->quote_symbol = make_symbol("quote");
  g->quasiquote_symbol = make_symbol("quasiquote");
  g->set_symbol = make_symbol("set!");
  g->if_symbol = make_symbol("if");
  g->begin_symbol = make_symbol("begin");
  g->lambda_symbol = make_symbol("lambda");
  g->macro_symbol = make_symbol("macro");
  g->stdin_symbol = make_symbol("stdin");
  g->stdout_symbol = make_symbol("stdout");
  g->stderr_symbol = make_symbol("stderr");
  g->exit_hook_symbol = make_symbol("exit-hook");
  g->error_sym = make_uninterned_symbol("error");
  push_root(&(g->error_sym));

  g->eof_object = alloc_object(0);
  g->eof_object->type = EOF_OBJECT;
  push_root(&(g->eof_object));

  g->empty_env = g->empty_list;
  g->env = make_hashtab(100);
  push_root(&(g->env));
  interp_definer("*global-environment*", g->env);


  g->vm_env = make_hashtab(100);
  push_root(&(g->vm_env));
  vm_definer("*global-environment*", g->vm_env);

  init_prim_environment(interp_definer);
  vm_init_environment(interp_definer);
  init_ffi(interp_definer);

  init_prim_environment(vm_definer);
  vm_init_environment(vm_definer);
  init_ffi(vm_definer);

  vm_init();

  interp_definer("*vm-global-environment*", g->vm_env);
}

void destroy_interp() {
  pop_root(&(g->env));
  g->env = g->empty_list;
}


/**
 * handy for user side debugging */
void print_obj(object * obj) {
  owrite(stdout, obj);
  printf("\n");
}

void primitive_repl() {
  object *input;
  while((input = obj_read(stdin)) != NULL) {
    push_root(&input);
    print_obj(interp(input, g->empty_env));
    pop_root(&input);
  }
}
