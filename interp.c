#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "types.h"
#include "interp.h"

#define DEBUG

/* dealing with environments */
object *enclosing_environment(object *env) {
  return cdr(env);
}

object *first_frame(object *env) {
  return car(env);
}

object *make_frame(object *variables, object *values) {
  return cons(variables, values);
}

object *frame_variables(object *frame) {
  return car(frame);
}

object *frame_values(object *frame) {
  return cdr(frame);
}

void add_binding_to_frame(object *var, object *val,
			  object *frame) {
  set_car(frame, cons(var, car(frame)));
  set_cdr(frame, cons(val, cdr(frame)));
}

object *extend_environment(object *vars, object *vals,
			   object *base_env) {
  return cons(make_frame(vars, vals), base_env);
}

/**
 * PRIVATE
 * @return cons cell where car(v) is variable value, nil on fail
 */
object *find_variable_value(object *var, object *env,
			    char search_enclosing) {
  object *frame;
  object *vars;
  object *vals;

  while(!is_the_empty_list(env)) {
    frame = first_frame(env);
    vars = frame_variables(frame);
    vals = frame_values(frame);
    while(!is_the_empty_list(vars)) {
      if(var == car(vars)) {
	return vals;
      }
      vars = cdr(vars);
      vals = cdr(vals);
    }
    if(!search_enclosing) return the_empty_list;
    env = enclosing_environment(env);
  }

  return the_empty_list;
}

/**
 * It turns out that find_variable_value is very useful for
 * implementing &rest args in userspace
 */
DEFUN1(find_variable_proc) {
  object *val = find_variable_value(FIRST, environment, 0);
  return val;
}

/**
 * PRIVATE
 * Handle errors when a symbol is assumed to be bound but isn't
 */
void throw_unbound_sym(object *sym) {
  fprintf(stderr, "unbound symbol, %s\n", sym->data.symbol.value);
  exit(1);
}

object *lookup_variable_value(object *var, object *env) {
  object *val = find_variable_value(var, env, 1);
  if(is_the_empty_list(val)) throw_unbound_sym(var);

  return car(val);
}

void set_variable_value(object *var, object *new_val, object *env) {
  object *val = find_variable_value(var, env, 1);
  if(is_the_empty_list(val)) throw_unbound_sym(var);

  set_car(val, new_val);
}

/**
 * like set_variable_value but we define the variable if it doesn't
 * exist in our current frame. Unlike set_variable_value we don't
 * search enclosing environments.
 */
void define_variable(object *var, object *new_val, object *env) {
  object *val = find_variable_value(var, env, 0);
  if(is_the_empty_list(val)) {
    add_binding_to_frame(var, new_val, first_frame(env));
  } else {
    set_car(val, new_val);
  }
}

/**
 * create a complete empty and unrooted environment
 */
object *setup_environment(void) {
  return extend_environment(the_empty_list,
			    the_empty_list,
			    the_empty_environment);
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
  return AS_BOOL(is_primitive_proc(FIRST) || is_compound_proc(FIRST));
}

DEFUN1(add_proc) {
  debug_write("prim-+", arguments, 0);

  long result = 0;
  while(!is_the_empty_list(arguments)) {
    result += LONG(FIRST);
    NEXT;
  }
  return make_fixnum(result);
}

DEFUN1(sub_proc) {
  long result = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    result -= LONG(FIRST);
  }
  return make_fixnum(result);
}

DEFUN1(mul_proc) {
  long result = 1;
  while(!is_the_empty_list(arguments)) {
    result *= LONG(FIRST);
    NEXT;
  }
  return make_fixnum(result);
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

DEFUN1(macroexpand0_proc) {
  return expand_macro(car(FIRST), cdr(FIRST), environment);
}

DEFUN1(is_eq_proc) {
  if(FIRST->type != SECOND->type) {
    return false;
  }
  switch(FIRST->type) {
  case FIXNUM:
    return (LONG(FIRST) == LONG(SECOND)) ? true : false;
  case CHARACTER:
    return (CHAR(FIRST) == CHAR(SECOND)) ? true : false;
  case STRING:
    return (strcmp(STRING(FIRST), STRING(SECOND)) == 0) ?
      true : false;
  default:
    return (FIRST == SECOND) ? true : false;
  }
}

DEFUN1(is_number_equal_proc) {
  long value = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    if(value != LONG(FIRST)) {
      return false;
    }
  }
  return true;
}

DEFUN1(is_less_than_proc) {
  long last = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    if(last < LONG(FIRST)) {
      last = LONG(FIRST);
    } else {
      return false;
    }
  }
  return true;
}

DEFUN1(is_greater_than_proc) {
  long last = LONG(FIRST);
  while(!is_the_empty_list(NEXT)) {
    if(last > LONG(FIRST)) {
      last = LONG(FIRST);
    } else {
      return false;
    }
  }
  return true;
}



void write_pair(FILE *out, object *pair) {
  object *car_obj = car(pair);
  object *cdr_obj = cdr(pair);

  write(out, car_obj);
  if(is_pair(cdr_obj)) {
    fprintf(out, " ");
    write_pair(out, cdr_obj);
  }
  else if(is_the_empty_list(cdr_obj)) {
    return;
  }
  else {
    fprintf(out, " . ");
    write(out, cdr_obj);
  }
}

void write(FILE *out, object *obj) {
  char c;
  char *str;
  object *head;

  switch(obj->type) {
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
    fprintf(out, "%ld", obj->data.fixnum.value);
    break;
  case CHARACTER:
    fprintf(out, "#\\");
    c = obj->data.character.value;
    switch(c) {
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
    while (*str != '\0') {
      switch(*str) {
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
  case PAIR:
    head = car(obj);
    if(head == quote_symbol) {
      fprintf(out, "'");
      write(out, cadr(obj));
    }
    else if(head == unquote_symbol) {
      fprintf(out, ",");
      write(out, cadr(obj));
    } else {
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
  case SYNTAX_PROC:
    fprintf(out, "#<syntax-procedure>");
    break;
  default:
    fprintf(stderr, "cannot write unknown type\n");
    exit(1);
  }
}

void throw_interp(char * msg, ...) {
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
  exit(1);
}

#ifdef DEBUG
void debug_write(char * msg, object *obj, int level) {
  int ii;
  for(ii = 0; ii < level; ++ii) {
    fprintf(stderr, " ");
  }

  fprintf(stderr, "%s: ", msg);
  write(stderr, obj);
  fprintf(stderr, "\n");
}
#else
void debug_write(char * msg, object *obj, int level) {
}
#endif

object *interp(object *exp, object *env) {
  return interp1(exp, env, 0);
}

object *expand_macro(object *macro, object *args, object *env) {
  object *new_env =
    extend_environment(macro->data.compound_proc.parameters,
		       args,
		       env);
  object *expanded = interp(macro->data.compound_proc.body, new_env);
  return expanded;
}

object *interp_unquote(object *exp, object *env, int level) {
  if(!is_pair(exp)) return exp;

  object *head = car(exp);
  if(head == unquote_symbol) {
    return interp1(second(exp), env, level);
  } else {
    return cons(interp_unquote(car(exp), env, level),
		interp_unquote(cdr(exp), env, level));
    }
}

object *interp1(object *exp, object *env, int level) {
  debug_write("interpreting", exp, level);

  if(is_symbol(exp)) {
    debug_write("looking up symbol", exp, level);
    object *result = lookup_variable_value(exp, env);
    debug_write("found", result, level);
    return result;
  }
  else if(is_atom(exp)) {
    debug_write("exp is atomic", exp, level);
    return exp;
  }
  else if(is_pair(exp)) {
    object *head = car(exp);
    if(head == quote_symbol) {
      debug_write("evaluating quote", exp, level);
      return interp_unquote(second(exp), env, level);
    }
    else if(head == begin_symbol) {
      debug_write("evaluating begin", exp, level);
      object *result;
      exp = cdr(exp);
      while(!is_the_empty_list(exp)) {
	result = interp1(car(exp), env, level + 1);
	exp = cdr(exp);
      }
      return result;
    }
    else if(head == set_symbol) {
      debug_write("evaluating set", exp, level);
      object *args = cdr(exp);
      object *val = interp1(second(args), env, level + 1);
      define_variable(first(args), val, env);
      return val;
    }
    else if(head == if_symbol) {
      debug_write("evaluating if", exp, level);
      object *args = cdr(exp);
      object *predicate = interp1(first(args), env, level + 1);
      if(predicate == true) {
	return interp1(second(args), env, level + 1);
      } else {
	return interp1(third(args), env, level + 1);
      }
    }
    else if(head == lambda_symbol) {
      debug_write("defining lambda", exp, level);
      object *args = cdr(exp);
      return make_compound_proc(first(args),
				cons(begin_symbol, cdr(args)),
				env);
    }
    else if(head == macro_symbol) {
      debug_write("defining macro", exp, level);
      object *args = cdr(exp);
      return make_syntax_proc(first(args),
			      cons(begin_symbol, cdr(args)));
    }
    else {
      /* procedure application */
      debug_write("applying. head is", head, level);
      object *fn = interp1(head, env, level + 1);
      debug_write("now have", fn, level);

      object *args = cdr(exp);
      object *evald_args;

      if(is_syntax_proc(fn)) {
	/* expand the macro and evaluate that */
	object *expanded = expand_macro(fn, args, env);
	debug_write("expanded macro", expanded, level);
	return interp1(expanded, env, level + 1);
      }

      /* evaluate the arguments */
      evald_args = the_empty_list;
      object *last;
      while(!is_the_empty_list(args)) {
	object *result = interp1(first(args), env, level + 1);
	if(evald_args == the_empty_list) {
	  evald_args = cons(result, the_empty_list);
	  last = evald_args;
	} else {
	  set_cdr(last, cons(result, the_empty_list));
	}
	args = cdr(args);
      }
    

      /* dispatch the call */
      if(is_primitive_proc(fn)) {
	debug_write("primitive apply", evald_args, level);
	return fn->data.primitive_proc.fn(evald_args, env);
      } else if(is_compound_proc(fn)) {
	object *new_env;
	debug_write("parameters", fn->data.compound_proc.parameters, level);
	debug_write("bindings", evald_args, level);
	new_env = extend_environment(fn->data.compound_proc.parameters,
				     evald_args,
				     fn->data.compound_proc.env);
	return interp1(fn->data.compound_proc.body, new_env, level + 1);
      } else {
	debug_write("error", fn, level);
	throw_interp("cannot apply non-function\n");
	return NULL;
      }
    }

  }

  write(stderr, exp);
  throw_interp(": can't evaluate\n");
  return NULL;
}


void init_prim_environment(object *env) {
#define add_procedure(scheme_name, c_name)    \
  define_variable(make_symbol(scheme_name),   \
                  make_primitive_proc(c_name),\
                  env);

  add_procedure("null?", is_null_proc);
  add_procedure("boolean?", is_boolean_proc);
  add_procedure("symbol?", is_symbol_proc);
  add_procedure("integer?", is_integer_proc);
  add_procedure("char?", is_char_proc);
  add_procedure("string?", is_string_proc);
  add_procedure("pair?", is_pair_proc);
  add_procedure("procedure?", is_procedure_proc);

  add_procedure("+", add_proc);
  add_procedure("-", sub_proc);
  add_procedure("*", mul_proc);
  add_procedure("<", is_less_than_proc);
  add_procedure(">", is_greater_than_proc);
  add_procedure("=", is_number_equal_proc);

  add_procedure("cons", cons_proc);
  add_procedure("car", car_proc);
  add_procedure("cdr", cdr_proc);
  add_procedure("set-car!", set_car_proc);
  add_procedure("set-cdr!", set_cdr_proc);
  add_procedure("list", list_proc);

  add_procedure("eq?", is_eq_proc);

  add_procedure("macroexpand0", macroexpand0_proc);

  add_procedure("find-variable", find_variable_proc);
}

void init() {
  the_empty_list = alloc_object();
  the_empty_list->type = THE_EMPTY_LIST;

  false = alloc_object();
  false->type = BOOLEAN;
  false->data.boolean.value = 0;

  true = alloc_object();
  true->type = BOOLEAN;
  true->data.boolean.value = 1;

  symbol_table = the_empty_list;
  unquote_symbol = make_symbol("unquote");
  quote_symbol = make_symbol("quote");
  set_symbol = make_symbol("set!");
  if_symbol = make_symbol("if");
  begin_symbol = make_symbol("begin");
  lambda_symbol = make_symbol("lambda");
  macro_symbol = make_symbol("macro");

  the_empty_environment = the_empty_list;
  the_global_environment = setup_environment();
  init_prim_environment(the_global_environment);
}

/**
 * handy for user side debugging */
void print_obj(object *obj) {
  write(stdout, obj);
  printf("\n");
}
