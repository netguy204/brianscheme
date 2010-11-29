#ifndef TYPES_H
#define TYPES_H

#include <stdio.h>

/* first implementing the classic tagged type. This specific
   inplementation is stolen from Peter Michaux's bootstrap-scheme. */

typedef enum {NIL, BOOLEAN, SYMBOL, FIXNUM,
	      CHARACTER, STRING, PAIR, PRIMITIVE_PROC,
	      COMPOUND_PROC, INPUT_PORT, OUTPUT_PORT,
	      EOF_OBJECT, THE_EMPTY_LIST} object_type;

typedef struct object {
  object_type type;
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
      struct object *(*fn)(struct object *arguments);
    } primitive_proc;
    struct {
      struct object *parameters;
      struct object *body;
      struct object *env;
    } compound_proc;
    struct {
      FILE *stream;
    } input_port;
    struct {
      FILE *stream;
    } output_port;
  } data;
} object;

/* global primitive symbols */

object *the_empty_list;
object *false;
object *true;
object *symbol_table;
object *quote_symbol;
object *set_symbol;
object *if_symbol;
object *begin_symbol;
object *lambda_symbol;

object *the_empty_environment;
object *the_global_environment;

/* some basic functions for dealing with tagged types */

void *MALLOC(long size);

object *alloc_object(void);

object *make_symbol(char *value);

char is_the_empty_list(object *obj);

char is_boolean(object *obj);

char is_false(object *obj);

char is_true(object *obj);

char is_symbol(object *obj);

object *make_fixnum(long value);

char is_fixnum(object *obj);

object *make_character(char value);

char is_character(object *obj);

object *make_string(char *value);

char is_string(object *obj);

object *cons(object *car, object *cdr);

char is_pair(object *obj);

object *car(object *pair);

void set_car(object *obj, object *value);

object *cdr(object *pair);

void set_cdr(object *obj, object *value);

#define caar(obj) car(car(obj))
#define cadr(obj) car(cdr(obj))
#define first(obj) car(obj)
#define second(obj) cadr(obj)
#define third(obj) car(cdr(cdr(obj)))

object *make_primitive_proc(object *(*fn)(struct object *arguments));
char is_primitive_proc(object *obj);
object *make_compound_proc(object *parameters, object *body,
			   object *env);
char is_compound_proc(object *obj);

char is_atom(object *obj);

#endif
