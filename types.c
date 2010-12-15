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

object *make_unfilled_vector(long size) {
  object *obj = alloc_object();
  obj->type = VECTOR;
  VARRAY(obj) = MALLOC(sizeof(object) * size);
  VSIZE(obj) = size;
  return obj;
}

object *make_vector(object *fill, long size) {
  int ii;
  object *obj = make_unfilled_vector(size);

  for(ii = 0; ii < size; ++ii) {
    VARRAY(obj)[ii] = fill;
  }
  return obj;
}

char is_vector(object *obj) {
  return obj->type == VECTOR;
}

object *list_to_vector(object *list) {
  long length = 0;
  long position = 0;

  object *next = list;

  if(is_the_empty_list(list)) {
    return the_empty_vector;
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

int hash_function(void *key, size_t keylen, size_t ht_size) {
  object *obj = (object*)key;
  long temp;
  int temp2;
  hashtab_iter_t htab_iter;

  switch(obj->type) {
  case BOOLEAN:
    return ht_hash(&obj->data.boolean.value, sizeof(char), ht_size);
  case SYMBOL:
    /* hash the address since it's intern'd */
    return ht_hash(&obj, sizeof(object*), ht_size);
  case FIXNUM:
    return ht_hash(&obj->data.fixnum.value, sizeof(long), ht_size);
  case CHARACTER:
    return ht_hash(&obj->data.character.value, sizeof(char), ht_size);
  case STRING:
    temp = strlen(obj->data.string.value);
    return ht_hash(obj->data.string.value, temp, ht_size);
  case PAIR:
    return hash_function(obj->data.pair.car, keylen, ht_size) +
      hash_function(obj->data.pair.cdr, keylen, ht_size);
  case VECTOR:
    temp2 = 0;
    for(temp = 0; temp < VSIZE(obj); ++temp) {
      temp2 += hash_function(VARRAY(obj)[temp], keylen, ht_size);
    }
    return temp2;
  case HASH_TABLE:
    ht_iter_init(HTAB(obj), &htab_iter);
    temp2 = 0;
    while(htab_iter.key != NULL) {
      temp2 += hash_function(htab_iter.key, keylen, ht_size) +
	hash_function(htab_iter.value, keylen, ht_size);
      ht_iter_inc(&htab_iter);
    }
    return temp2;
  case PRIMITIVE_PROC:
    return ht_hash(&obj->data.primitive_proc.fn,
		   sizeof(obj->data.primitive_proc.fn), ht_size);
  case COMPOUND_PROC:
    return hash_function(obj->data.compound_proc.parameters, keylen, ht_size) +
      hash_function(obj->data.compound_proc.body, keylen, ht_size) +
      hash_function(obj->data.compound_proc.env, keylen, ht_size);
  case COMPILED_PROC:
    return hash_function(BYTECODE(obj), keylen, ht_size) +
      hash_function(CENV(obj), keylen, ht_size);
  case INPUT_PORT:
  case OUTPUT_PORT:
    return ht_hash(&obj->data.input_port.stream, sizeof(FILE*), ht_size);
  default:
    return 0;
  }
}

object *make_hashtab(long size) {
  object *obj = alloc_object();
  obj->type = HASH_TABLE;
  HTAB(obj) = ht_init(size, hash_function);
  return obj;
}

char is_hashtab(object *obj) {
  return obj->type == HASH_TABLE;
}

void set_hashtab(object *tab, object *key, object *val) {
  ht_insert(HTAB(tab), key, sizeof(object*),
	    val, sizeof(object*));
}

void remkey_hashtab(object *tab, object *key) {
  ht_remove(HTAB(tab), key, sizeof(object*));
}

object *get_hashtab(object *tab, object *key, object *fail) {
  object *val = ht_search(HTAB(tab), key, sizeof(object*));
  if(val == NULL) {
    return fail;
  } else {
    return val;
  }
}

object *get_hashtab_keys(object *table) {
  object *result = the_empty_list;
  push_root(&result);

  hashtab_iter_t iter;
  ht_iter_init(HTAB(table), &iter);
  while(iter.key != NULL) {
    result = cons((object*)iter.key, result);
    ht_iter_inc(&iter);
  }
  pop_root(&result);
  return result;
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

object *make_compiled_proc(object *bytecode, object *env) {
  object *obj = alloc_object();

  obj->type = COMPILED_PROC;
  obj->data.compiled_proc.bytecode = bytecode;
  obj->data.compiled_proc.env = env;
  return obj;
}

char is_compiled_proc(object *obj) {
  return obj->type == COMPILED_PROC;
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

object *make_uninterned_symbol(char *value) {
  object *obj = alloc_object();
  obj->type = SYMBOL;
  obj->data.symbol.value = MALLOC(strlen(value) + 1);
  strcpy(obj->data.symbol.value, value);
  return obj;
}

object *make_symbol(char *value) {
  object *obj;
  object *element;

  element = find_symbol(value);
  if(element != NULL) return car(element);

  obj = make_uninterned_symbol(value);

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
