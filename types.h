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

#ifndef TYPES_H
#define TYPES_H

#include <stdio.h>
#include "hashtab.h"

/* first implementing the classic tagged type. This specific
   inplementation is stolen from Peter Michaux's bootstrap-scheme. */

typedef enum {NIL, BOOLEAN, SYMBOL, FIXNUM, FLOATNUM,
	      CHARACTER, STRING, PAIR, PRIMITIVE_PROC,
	      COMPOUND_PROC, INPUT_PORT, OUTPUT_PORT,
	      EOF_OBJECT, THE_EMPTY_LIST, SYNTAX_PROC,
	      COMPILED_SYNTAX_PROC, VECTOR, COMPILED_PROC,
	      HASH_TABLE, ALIEN, META_PROC} object_type;

typedef struct object {
  char color;
  /* garbage collection data */
  object_type type;
  struct object* next;
  struct object* prev;

  union {
    struct {
      char value;
    } boolean;
    struct {
      char * value;
    } symbol;
    struct {
      long value;
    } fixnum;
    struct {
      double value;
    } floatnum;
    struct {
      char value;
    } character;
    struct {
      char *value;
    } string;
    struct {
      struct object *car;
      struct object *cdr;
    } pair;
    struct {
      struct object **objects;
      long size;
    } vector;
    struct {
      struct hashtab_t* hash_table;
    } hash_table;
    struct {
      struct object *(*fn)(struct object *argstack,
			   long n, long top);
    } primitive_proc;
    struct {
      struct object *parms_and_env;
      struct object *body;
    } compound_proc; /* also syntax */
    struct {
      struct object *bytecode;
      struct object *env;
    } compiled_proc;
    struct {
      FILE *stream;
    } input_port;
    struct {
      FILE *stream;
    } output_port;
    struct {
      struct object *releaser;
      union {
	void *ptr;
	void (*fn_ptr)(void);
      } data;
    } alien;
    struct {
      struct object *proc;
      struct object *data;
    } meta_proc;
  } data;
} object;

typedef struct object* (prim_proc)(struct object*,
				   long, long);

/* some basic functions for dealing with tagged types */

void *MALLOC(long size);

object *alloc_object(char needs_finalization);

object *make_uninterned_symbol(char *value);

object *make_symbol(char *value);

char is_the_empty_list(object *obj);

char is_boolean(object *obj);

char is_false(object *obj);

char is_true(object *obj);

char is_symbol(object *obj);

object *make_fixnum(long value);

char is_fixnum(object *obj);

object *make_real(double value);

char is_real(object *obj);

object *make_character(char value);

char is_character(object *obj);

object *make_empty_string(long length);

object *make_string(char *value);

char is_string(object *obj);

object *cons(object *car, object *cdr);

char is_pair(object *obj);

object *car(object *pair);

void set_car(object *obj, object *value);

object *cdr(object *pair);

void set_cdr(object *obj, object *value);

object *make_vector(object *fill, long size);

object *list_to_vector(object *list);

object *make_hashtab(long size);

char is_hashtab(object *obj);

void set_hashtab(object *table, object *key, object *value);

object *get_hashtab(object *table, object *key, object *fail);

object *get_hashtab_keys(object *table);

void remkey_hashtab(object *table, object *key);

char is_vector(object *obj);

char is_alien(object *obj);

object *make_alien(void * ptr, object * releaser);

object *make_alien_fn(void (*fn)(void), object * releaser);

char is_meta(object *obj);

object *make_meta_proc(object *proc, object *meta);

/* statistics */
long get_alloc_count();
long get_cons_count();

#define caar(obj) car(car(obj))
#define cadr(obj) car(cdr(obj))
#define cddr(obj) cdr(cdr(obj))
#define cdddr(obj) cdr(cddr(obj))
#define cddddr(obj) cdr(cdddr(obj))

#define caddr(obj) car(cddr(obj))
#define cadddr(obj) car(cdddr(obj))
#define caddddr(obj) car(cddddr(obj))

#define first(obj) car(obj)
#define second(obj) cadr(obj)
#define third(obj) caddr(obj)
#define fourth(obj) cadddr(obj)
#define fifth(obj) caddddr(obj)

#define list1(a) cons(a,the_empty_list)
#define list2(a,b) cons(a,list1(b))
#define list3(a,b,c) cons(a,list2(b,c))
#define list4(a,b,c,d) cons(a,list3(b,c,d))
#define list5(a,b,c,d,e) cons(a,list4(b,c,d,e))

#define CAR(x) (x->data.pair.car)
#define CDR(x) (x->data.pair.cdr)
#define LONG(x) (x->data.fixnum.value)
#define DOUBLE(x) (x->data.floatnum.value)
#define CHAR(x) (x->data.character.value)
#define STRING(x) (x->data.string.value)
#define SYMBOL(x) (x->data.symbol.value)
#define BOOLEAN(x) (x->data.boolean.value)
#define INPUT(x) (x->data.input_port.stream)
#define OUTPUT(x) (x->data.output_port.stream)
#define COMPOUND_BODY(x) (x->data.compound_proc.body)
#define COMPOUND_PARMS_AND_ENV(x) (x->data.compound_proc.parms_and_env)
#define COMPOUND_PARAMS(x) (CAR(COMPOUND_PARMS_AND_ENV(x)))
#define COMPOUND_ENV(x) (CDR(COMPOUND_PARMS_AND_ENV(x)))
#define VARRAY(obj) (obj->data.vector.objects)
#define VSIZE(obj) (obj->data.vector.size)
#define BYTECODE(obj) (obj->data.compiled_proc.bytecode)
#define CENV(obj) (obj->data.compiled_proc.env)

#define HTAB(obj) (obj->data.hash_table.hash_table)
#define ALIEN_RELEASER(obj) (obj->data.alien.releaser)
#define ALIEN_PTR(obj) (obj->data.alien.data.ptr)
#define ALIEN_FN_PTR(obj) (obj->data.alien.data.fn_ptr)
#define METAPROC(obj) (obj->data.meta_proc.proc)
#define METADATA(obj) (obj->data.meta_proc.data)

/* some gcc specific magic so that we can give the compiler hints
   about what sides of a branch to optimize for */
#define likely(x) __builtin_expect((x), 1)
#define unlikely(x) __builtin_expect((x), 0)

object *make_primitive_proc(prim_proc fn);
char is_primitive_proc(object *obj);

object *make_compound_proc(object *parameters, object *body,
			   object *env);

char is_compound_proc(object *obj);
char is_syntax_proc(object *obj);
char is_compiled_syntax_proc(object *obj);

object *make_compiled_proc(object *bytecode, object *env);

char is_compiled_proc(object *obj);

object *make_input_port(FILE *stream);
object *make_output_port(FILE *stream);
char is_input_port(object *obj);
char is_output_port(object *obj);
char is_eof_object(object *obj);

char is_atom(object *obj);

#endif
