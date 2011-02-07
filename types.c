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

#include "types.h"
#include "gc.h"

char is_the_empty_list(object * obj) {
  return obj == g->empty_list;
}

char is_boolean(object * obj) {
  return obj->type == BOOLEAN;
}

char is_false(object * obj) {
  return obj == g->false;
}

char is_true(object * obj) {
  return obj == g->true;
}

char is_symbol(object * obj) {
  return obj->type == SYMBOL;
}

object *make_fixnum(long value) {
  object *obj = alloc_object(0);
  obj->type = FIXNUM;
  LONG(obj) = value;
  return obj;
}

char is_fixnum(object * obj) {
  return obj->type == FIXNUM;
}

object *make_real(double value) {
  object *obj = alloc_object(0);
  obj->type = FLOATNUM;
  DOUBLE(obj) = value;
  return obj;
}

char is_real(object * obj) {
  return obj->type == FLOATNUM;
}

object *make_character(char value) {
  object *obj = alloc_object(0);
  obj->type = CHARACTER;
  obj->data.character.value = value;
  return obj;
}

char is_character(object * obj) {
  return obj->type == CHARACTER;
}

object *make_empty_string(long len) {
  object *obj = alloc_object(1);
  obj->type = STRING;
  obj->data.string.value = MALLOC(len);
  return obj;
}

object *make_filled_string(long length, char fill_char) {
  int idx;
  object *string = make_empty_string(length);
  for(idx = 0; idx < length - 1; ++idx) {
    STRING(string)[idx] = fill_char;
  }
  return string;
}

object *make_string(char *value) {
  size_t len = strlen(value) + 1;
  object *obj = make_empty_string(len);
  strncpy(obj->data.string.value, value, len);
  return obj;
}

char is_string(object * obj) {
  return obj->type == STRING;
}

static long Cons_Count = 0;
object *cons(object * car, object * cdr) {
  object *obj = alloc_object(0);
  obj->type = PAIR;
  obj->data.pair.car = car;
  obj->data.pair.cdr = cdr;
  ++Cons_Count;
  return obj;
}

long get_cons_count() {
  return Cons_Count;
}

char is_pair(object * obj) {
  return obj->type == PAIR;
}

object *car(object * pair) {
  return CAR(pair);
}

void set_car(object * obj, object * value) {
  CAR(obj) = value;
}

object *cdr(object * pair) {
  return CDR(pair);
}

void set_cdr(object * obj, object * value) {
  CDR(obj) = value;
}

object *make_unfilled_vector(long size) {
  object *obj = alloc_object(1);
  obj->type = VECTOR;
  VARRAY(obj) = MALLOC(sizeof(object) * size);
  VSIZE(obj) = size;
  return obj;
}

object *make_vector(object * fill, long size) {
  int ii;
  object *obj = make_unfilled_vector(size);

  for(ii = 0; ii < size; ++ii) {
    VARRAY(obj)[ii] = fill;
  }
  return obj;
}

char is_vector(object * obj) {
  return obj->type == VECTOR;
}

char is_alien(object * obj) {
  return obj->type == ALIEN;
}

object *make_alien(void *ptr, object * releaser) {
  object *obj = alloc_object(0);
  obj->type = ALIEN;
  ALIEN_PTR(obj) = ptr;
  ALIEN_RELEASER(obj) = releaser;
  return obj;
}

object *make_alien_fn(void (*fn) (void), object * releaser) {
  object *obj = alloc_object(0);
  obj->type = ALIEN;
  ALIEN_FN_PTR(obj) = fn;
  ALIEN_RELEASER(obj) = releaser;
  return obj;
}

char is_meta(object * obj) {
  return obj->type == META_PROC;
}

object *make_meta_proc(object * proc, object * meta) {
  object *obj = alloc_object(0);
  obj->type = META_PROC;
  METAPROC(obj) = proc;
  METADATA(obj) = meta;
  return obj;
}

object *list_to_vector(object * list) {
  long length = 0;
  long position = 0;

  object *next = list;

  if(is_the_empty_list(list)) {
    return g->empty_vector;
  }

  while(!is_the_empty_list(next)) {
    ++length;
    next = cdr(next);
  }

  object *vector = make_unfilled_vector(length);
  next = list;

  while(!is_the_empty_list(next)) {
    VARRAY(vector)[position++] = car(next);
    next = cdr(next);
  }

  return vector;
}

object *make_hashtab(long size) {
  object *obj = alloc_object(1);
  obj->type = HASH_TABLE;
  HTAB(obj) = htb_init(size, NULL);
  return obj;
}

char is_hashtab(object * obj) {
  return obj->type == HASH_TABLE;
}

void set_hashtab(object * tab, object * key, object * val) {
  htb_insert(HTAB(tab), key, val);
}

void remkey_hashtab(object * tab, object * key) {
  htb_remove(HTAB(tab), key);
}

object *get_hashtab(object * tab, object * key, object * fail) {
  object *val = htb_search(HTAB(tab), key);
  if(val == NULL) {
    return fail;
  }
  else {
    return val;
  }
}

object *get_hashtab_keys(object * table) {
  object *result = g->empty_list;
  push_root(&result);

  hashtab_iter_t iter;
  htb_iter_init(HTAB(table), &iter);
  while(iter.key != NULL) {
    result = cons((object *) iter.key, result);
    htb_iter_inc(&iter);
  }
  pop_root(&result);
  return result;
}

object *make_primitive_proc(prim_proc fn) {
  object *obj = alloc_object(0);
  obj->type = PRIMITIVE_PROC;
  obj->data.primitive_proc.fn = fn;
  return obj;
}

char is_primitive_proc(object * obj) {
  return obj->type == PRIMITIVE_PROC;
}

object *make_compound_proc(object * parameters, object * body, object * env) {

  object *obj = alloc_object(0);
  obj->type = COMPOUND_PROC;
  COMPOUND_PARMS_AND_ENV(obj) = g->empty_list;
  COMPOUND_BODY(obj) = body;

  push_root(&obj);
  COMPOUND_PARMS_AND_ENV(obj) = cons(parameters, env);
  pop_root(&obj);

  return obj;
}

char is_compound_proc(object * obj) {
  return obj->type == COMPOUND_PROC;
}

char is_syntax_proc(object * obj) {
  return obj->type == SYNTAX_PROC;
}

char is_compiled_syntax_proc(object * obj) {
  return obj->type == COMPILED_SYNTAX_PROC;
}

object *make_compiled_proc(object * bytecode, object * env) {
  object *obj = alloc_object(0);

  obj->type = COMPILED_PROC;
  BYTECODE(obj) = bytecode;
  CENV(obj) = env;

  return obj;
}

char is_compiled_proc(object * obj) {
  return obj->type == COMPILED_PROC;
}

object *make_input_port(FILE * stream, char is_pipe) {
  object *obj = alloc_object(0);

  obj->type = INPUT_PORT;
  obj->data.input_port.stream = stream;
  obj->data.input_port.is_pipe = is_pipe;
  obj->data.input_port.opened = 1;
  return obj;
}

char is_input_port(object * obj) {
  return obj->type == INPUT_PORT;
}

char is_output_port(object * obj) {
  return obj->type == OUTPUT_PORT;
}

char is_output_port_pipe(object * obj) {
  return obj->data.output_port.is_pipe;
}

char is_input_port_pipe(object * obj) {
  return obj->data.input_port.is_pipe;
}

char is_output_port_opened(object * obj) {
  return obj->data.output_port.opened;
}

char is_input_port_opened(object * obj) {
  return obj->data.input_port.opened;
}

void set_output_port_opened(object * obj, char opened) {
  obj->data.output_port.opened = opened;
}

void set_input_port_opened(object * obj, char opened) {
  obj->data.input_port.opened = opened;
}

char is_dir_stream(object * obj) {
  return obj->type == DIR_STREAM;
}

char is_eof_object(object * obj) {
  return obj == g->eof_object;
}

object *make_output_port(FILE * stream, char is_pipe) {
  object *obj = alloc_object(0);

  obj->type = OUTPUT_PORT;
  obj->data.output_port.stream = stream;
  obj->data.output_port.is_pipe = is_pipe;
  obj->data.output_port.opened = 1;
  return obj;
}

object *make_dir_stream(DIR * stream) {
  object *obj = alloc_object(0);

  obj->type = DIR_STREAM;
  obj->data.dir.stream = stream;
  return obj;
}

object *find_symbol(char *value) {
  object *element;

  element = g->symbol_table;
  while(!is_the_empty_list(element)) {
    if(strcmp(car(element)->data.symbol.value, value) == 0) {
      return element;
    }
    element = cdr(element);
  }

  return NULL;
}

object *make_uninterned_symbol(char *value) {
  size_t len = strlen(value) + 1;
  object *obj = alloc_object(0);
  obj->type = SYMBOL;
  obj->data.symbol.value = MALLOC(len);
  strncpy(obj->data.symbol.value, value, len);
  return obj;
}

object *make_symbol(char *value) {
  object *obj;
  object *element;

  element = find_symbol(value);
  if(element != NULL)
    return car(element);

  obj = make_uninterned_symbol(value);

  push_root(&obj);
  g->symbol_table = cons(obj, g->symbol_table);
  pop_root(&obj);

  return obj;
}

char is_atom(object * obj) {
  return !is_pair(obj) || is_the_empty_list(obj);
}

object *make_primitive_exception(object *contents) {
  return cons(g->error_sym, contents);
}

char is_primitive_exception(object *obj) {
  return is_pair(obj) &&
    CAR(obj) == g->error_sym;
}

object *throw_message(char *msg, ...) {
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

