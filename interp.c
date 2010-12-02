#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "types.h"
#include "interp.h"
#include "gc.h"

static const int DEBUG_LEVEL = 1;

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
  object *result;

  result = make_frame(vars, vals);
  push_root(&result);
  result = cons(result, base_env);
  pop_root(&result);

  return result;
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
 * exist in our current frame.
 */
void define_variable(object *var, object *new_val, object *env) {
  object *val = find_variable_value(var, env, 1);
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

DEFUN1(is_output_port_proc) {
  return AS_BOOL(is_output_port(FIRST));
}

DEFUN1(is_input_port_proc) {
  return AS_BOOL(is_input_port(FIRST));
}

DEFUN1(is_eof_proc) {
  return AS_BOOL(is_eof_object(FIRST));
}

DEFUN1(add_proc) {
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
  object *macro = FIRST;
  object *macrofn = interp(car(macro), environment);
  object *macroargs = cdr(macro);

  push_root(&macrofn);
  object *result = expand_macro(macrofn, macroargs, environment, 0);
  pop_root(&macrofn);

  return result;
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

DEFUN1(open_output_port_proc) {
  object *name = FIRST;
  FILE *out = fopen(STRING(name), "w");
  if(out == NULL) {
    fprintf(stderr, "could not open file '%s' for output\n",
	    STRING(name));
    return the_empty_list;
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
    fprintf(stderr, "could not open file '%s' for input\n",
	    STRING(name));
    return the_empty_list;
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
  } else {
    env = SECOND;
  }

  return interp(exp, env);
}

object *read(FILE *in);
DEFUN1(read_proc) {
  object *in_port = FIRST;
  object *result = read(INPUT(in_port));
  return (result == NULL) ? eof_object : result;
}

DEFUN1(write_proc) {
  object *port = FIRST;
  object *obj = SECOND;
  write(OUTPUT(port), obj);
  return true;
}

DEFUN1(write_char_proc) {
  object *port = FIRST;
  object *ch = SECOND;
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

DEFUN1(number_to_string_proc) {
  char buffer[100];
  sprintf(buffer, "%ld", LONG(FIRST));
  return make_string(buffer);
}

DEFUN1(string_to_number_proc) {
  return make_fixnum(atoi(STRING(FIRST)));
}

DEFUN1(symbol_to_string_proc) {
  return make_string(FIRST->data.symbol.value);
}

DEFUN1(string_to_symbol_proc) {
  return make_symbol(STRING(FIRST));
}

DEFUN1(exit_proc) {
  exit((int)LONG(FIRST));
  return NULL;
}

DEFUN1(apply_proc) {
  object *fn = FIRST;
  object *args = SECOND;

  object *exp = cons(fn, args);
  push_root(&exp);
  object *result = interp(exp, environment);
  pop_root(&exp);
  return result;
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
  return make_fixnum(mark_and_sweep());
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

  if(obj == the_global_environment) {
    fprintf(out, "#<global-environment>");
    return;
  }

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
  case INPUT_PORT:
    fprintf(out, "#<input-port>");
    break;
  case OUTPUT_PORT:
    fprintf(out, "#<output-port>");
    break;
  case EOF_OBJECT:
    fprintf(out, "#<eof");
    break;
  default:
    fprintf(stderr, "cannot write unknown type: %d\n", obj->type);
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

object *debug_write(char * msg, object *obj, int level) {
  object *debug = lookup_variable_value(debug_symbol, the_global_environment);

  if(debug == true) {
    int ii;
    for(ii = 0; ii < level; ++ii) {
      fprintf(stderr, " ");
    }
    
    fprintf(stderr, "%s: ", msg);
    write(stderr, obj);
    fprintf(stderr, "\n");
  }
  return obj;
}

#ifdef NODEBUG
#define DN(msg, obj, level, n) obj
#else
#define DN(msg, obj, level, n)			\
  (DEBUG_LEVEL >= n ? debug_write(msg, obj, level) : obj);
#endif

#define D1(msg, obj, level) DN(msg, obj, level, 1);
#define D2(msg, obj, level) DN(msg, obj, level, 2);

object *interp(object *exp, object *env) {
  object *result = interp1(exp, env, 0);
  return result;
}

object *expand_macro(object *macro, object *args,
		     object *env, int level) {
  object *new_env =
    extend_environment(macro->data.compound_proc.parameters,
		       args,
		       env);
  push_root(&new_env);
  object *expanded = interp1(macro->data.compound_proc.body,
			     new_env, level);
  pop_root(&new_env);

  return expanded;
}

object *interp_unquote(object *exp, object *env, int level) {
  if(!is_pair(exp)) return exp;

  object *head = car(exp);
  if(head == unquote_symbol) {
    return interp1(second(exp), env, level);
  } else {
    object *h1 = interp_unquote(car(exp), env, level);
    push_root(&h1);

    object *h2 = interp_unquote(cdr(exp), env, level);
    push_root(&h2);

    object *result = cons(h1, h2);

    pop_root(&h2);
    pop_root(&h1);
    return result;		
  }
}

/* convenient tool for unbinding the arguments that must be bound
 * during interp1. We rebind to temp to force the evalation of result
 * if it's an exp
 */
#define INTERP_RETURN(result) \
  do {			      \
    object *temp = result;    \
    pop_root(&env);	      \
    pop_root(&exp);	      \
    return temp;	      \
  } while(0)

object *interp1(object *exp, object *env, int level) {
  /* we break the usual convention of assuming our own arguments are
   * protected here because the tail recursive call can rebind these
   * two items to something new
   */
  push_root(&exp);
  push_root(&env);

 interp_restart:
  D1("interpreting", exp, level);

  if(is_symbol(exp)) {
    INTERP_RETURN(lookup_variable_value(exp, env));
  }
  else if(is_atom(exp)) {
    INTERP_RETURN(exp);
  }
  else if(is_pair(exp)) {
    object *head = car(exp);
    if(head == quote_symbol) {
      INTERP_RETURN(interp_unquote(second(exp), env, level));
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

      if(predicate == true) {
	exp = second(args);
      } else {
	exp = third(args);
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

      if(is_syntax_proc(fn)) {
	/* expand the macro and evaluate that */
	exp = expand_macro(fn, args, env, level);
	pop_root(&fn);
	goto interp_restart;
      }

      /* evaluate the arguments */
      object *evald_args = the_empty_list;
      object *result = the_empty_list;
      object *last;
      push_root(&evald_args);
      push_root(&result);

      while(!is_the_empty_list(args)) {
	result = interp1(first(args), env, level + 1);

	if(evald_args == the_empty_list) {
	  evald_args = cons(result, the_empty_list);
	  last = evald_args;
	} else {
	  set_cdr(last, cons(result, the_empty_list));
	  last = cdr(last);
	}
	args = cdr(args);
      }
    
      /* dispatch the call */
      if(is_primitive_proc(fn)) {
	pop_root(&result);
	pop_root(&evald_args);
	pop_root(&fn);
	INTERP_RETURN(fn->data.primitive_proc.fn(evald_args, env));
      } else if(is_compound_proc(fn)) {
	env = extend_environment(fn->data.compound_proc.parameters,
				 evald_args,
				 fn->data.compound_proc.env);
	exp = fn->data.compound_proc.body;

	pop_root(&result);
	pop_root(&evald_args);
	pop_root(&fn);
	goto interp_restart;
      } else {
	pop_root(&result);
	pop_root(&evald_args);
	pop_root(&fn);

	debug_write("error", fn, level);
	throw_interp("cannot apply non-function\n");
	INTERP_RETURN(NULL);
      }
    }
  }

  write(stderr, exp);
  throw_interp(": can't evaluate\n");
  INTERP_RETURN(NULL);
}


void init_prim_environment(object *env) {
  /* used throughout this method to protect the thing in definition
   * from gc */
  object *curr = NULL;
  push_root(&curr);

#define add_procedure(scheme_name, c_name)    \
  define_variable(make_symbol(scheme_name),   \
                  curr=make_primitive_proc(c_name),	\
                  env);

  add_procedure("null?", is_null_proc);
  add_procedure("boolean?", is_boolean_proc);
  add_procedure("symbol?", is_symbol_proc);
  add_procedure("integer?", is_integer_proc);
  add_procedure("char?", is_char_proc);
  add_procedure("string?", is_string_proc);
  add_procedure("pair?", is_pair_proc);
  add_procedure("procedure?", is_procedure_proc);
  add_procedure("output-port?", is_output_port_proc);
  add_procedure("input-port?", is_input_port_proc);
  add_procedure("eof-object?", is_eof_proc);

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

  add_procedure("open-output-port", open_output_port_proc);
  add_procedure("open-input-port", open_input_port_proc);
  add_procedure("close-output-port", close_output_port_proc);
  add_procedure("close-input-port", close_input_port_proc);

  add_procedure("write-port", write_proc);
  add_procedure("read-port", read_proc);
  add_procedure("read-char", read_char_proc);
  add_procedure("write-char", write_char_proc);
  add_procedure("unread-char", unread_char_proc);

  add_procedure("macroexpand0", macroexpand0_proc);
  add_procedure("eval", eval_proc);
  add_procedure("apply", apply_proc);

  add_procedure("char->integer", char_to_integer_proc);
  add_procedure("number->string", number_to_string_proc);
  add_procedure("string->number", string_to_symbol_proc);
  add_procedure("symbol->string", symbol_to_string_proc);
  add_procedure("string->symbol", string_to_symbol_proc);

  add_procedure("find-variable", find_variable_proc);
  add_procedure("exit", exit_proc);
  add_procedure("interpreter-stats", stats_proc);
  add_procedure("mark-and-sweep", mark_and_sweep_proc);

  define_variable(debug_symbol, false, env);
  define_variable(stdin_symbol,
		  curr = make_input_port(stdin),
		  env);
  define_variable(stdout_symbol,
		  curr = make_output_port(stdout),
		  env);
  define_variable(stderr_symbol,
		  curr = make_output_port(stderr),
		  env);
  define_variable(make_symbol("base-env"),
		  env,
		  env);

  pop_root(&curr);
}

void init() {
  the_empty_list = alloc_object();
  the_empty_list->type = THE_EMPTY_LIST;
  push_root(&the_empty_list);

  false = alloc_object();
  false->type = BOOLEAN;
  false->data.boolean.value = 0;
  push_root(&false);

  true = alloc_object();
  true->type = BOOLEAN;
  true->data.boolean.value = 1;
  push_root(&true);

  symbol_table = the_empty_list;
  push_root(&symbol_table);

  unquote_symbol = make_symbol("unquote");
  quote_symbol = make_symbol("quote");
  set_symbol = make_symbol("set!");
  if_symbol = make_symbol("if");
  begin_symbol = make_symbol("begin");
  lambda_symbol = make_symbol("lambda");
  macro_symbol = make_symbol("macro");
  rest_symbol = make_symbol("&rest");
  debug_symbol = make_symbol("*debug*");
  stdin_symbol = make_symbol("stdin");
  stdout_symbol = make_symbol("stdout");
  stderr_symbol = make_symbol("stderr");

  eof_object = alloc_object();
  eof_object->type = EOF_OBJECT;
  push_root(&eof_object);

  the_empty_environment = the_empty_list;
  the_global_environment = setup_environment();
  push_root(&the_global_environment);

  init_prim_environment(the_global_environment);
}

/**
 * handy for user side debugging */
void print_obj(object *obj) {
  write(stdout, obj);
  printf("\n");
}