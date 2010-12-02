#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "symbols.h"
#include "gc.h"

char is_the_empty_list(object *obj) {
  return obj == the_empty_list;
}

char is_boolean(object *obj) {
  return obj->type == BOOLEAN;
}

char is_false(object *obj) {
  return obj == false;
}

char is_true(object *obj) {
  return obj == true;
}

char is_symbol(object *obj) {
  return obj->type == SYMBOL;
}

object *make_fixnum(long value) {
  object *obj = alloc_object();
  obj->type = FIXNUM;
  obj->data.fixnum.value = value;
  return obj;
}

char is_fixnum(object *obj) {
  return obj->type == FIXNUM;
}

object *make_character(char value) {
  object *obj = alloc_object();
  obj->type = CHARACTER;
  obj->data.character.value = value;
  return obj;
}

char is_character(object *obj) {
  return obj->type == CHARACTER;
}

object *make_string(char *value) {
  object *obj = alloc_object();
  obj->type = STRING;
  obj->data.string.value = MALLOC(strlen(value) + 1);
  strcpy(obj->data.string.value, value);
  return obj;
}

char is_string(object *obj) {
  return obj->type == STRING;
}

static long Cons_Count = 0;
object *cons(object *car, object *cdr) {
  object *obj = alloc_object();
  obj->type = PAIR;
  obj->data.pair.car = car;
  obj->data.pair.cdr = cdr;
  ++Cons_Count;
  return obj;
}

long get_cons_count() {
  return Cons_Count;
}

char is_pair(object *obj) {
  return obj->type == PAIR;
}

object *car(object *pair) {
  return pair->data.pair.car;
}

void set_car(object *obj, object *value) {
  obj->data.pair.car = value;
}

object *cdr(object *pair) {
  return pair->data.pair.cdr;
}

void set_cdr(object *obj, object *value) {
  obj->data.pair.cdr = value;
}

object *make_primitive_proc(prim_proc fn) {
  object *obj = alloc_object();
  obj->type = PRIMITIVE_PROC;
  obj->data.primitive_proc.fn = fn;
  return obj;
}

char is_primitive_proc(object *obj) {
  return obj->type == PRIMITIVE_PROC;
}

object *make_compound_proc(object *parameters, object *body,
			   object *env) {
  object *obj = alloc_object();

  obj->type = COMPOUND_PROC;
  obj->data.compound_proc.parameters = parameters;
  obj->data.compound_proc.body = body;
  obj->data.compound_proc.env = env;
  return obj;
}

char is_compound_proc(object *obj) {
  return obj->type == COMPOUND_PROC;
}

object *make_syntax_proc(object *parameters, object *body) {
  object *obj = alloc_object();

  obj->type = SYNTAX_PROC;
  obj->data.compound_proc.parameters = parameters;
  obj->data.compound_proc.body = body;
  obj->data.compound_proc.env = the_empty_environment;
  return obj;
}

char is_syntax_proc(object *obj) {
  return obj->type == SYNTAX_PROC;
}

object *make_input_port(FILE *stream) {
  object *obj = alloc_object();

  obj->type = INPUT_PORT;
  obj->data.input_port.stream = stream;
  return obj;
}

char is_input_port(object *obj) {
  return obj->type == INPUT_PORT;
}

char is_output_port(object *obj) {
  return obj->type == OUTPUT_PORT;
}

char is_eof_object(object *obj) {
  return obj == eof_object;
}

object *make_output_port(FILE *stream) {
  object *obj = alloc_object();

  obj->type = OUTPUT_PORT;
  obj->data.output_port.stream = stream;
  return obj;
}

object *find_symbol(char *value) {
  object *element;

  element = symbol_table;
  while(!is_the_empty_list(element)) {
    if(strcmp(car(element)->data.symbol.value, value) == 0) {
      return element;
    }
    element = cdr(element);
  }

  return NULL;
}

object *make_symbol(char *value) {
  object *obj;
  object *element;

  element = find_symbol(value);
  if(element != NULL) return car(element);

  obj = alloc_object();

  obj->type = SYMBOL;
  obj->data.symbol.value = MALLOC(strlen(value) + 1);
  strcpy(obj->data.symbol.value, value);

  push_root(&obj);
  symbol_table = cons(obj, symbol_table);
  pop_root(&obj);

  return obj;
}

char is_atom(object *obj) {
  return is_boolean(obj) ||
    is_fixnum(obj) ||
    is_character(obj) ||
    is_string(obj) ||
    is_compound_proc(obj) ||
    is_primitive_proc(obj) ||
    is_syntax_proc(obj);
}