#ifndef TYPES_H
#define TYPES_H

#include <stdio.h>

/* first implementing the classic tagged type. This specific
   inplementation is stolen from Peter Michaux's bootstrap-scheme. */

typedef enum {NIL, BOOLEAN, SYMBOL, FIXNUM,
	      CHARACTER, STRING, PAIR, PRIMITIVE_PROC,
	      COMPOUND_PROC, INPUT_PORT, OUTPUT_PORT,
	      EOF_OBJECT, THE_EMPTY_LIST, SYNTAX_PROC,
	      VECTOR} object_type;

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
      struct object **objects;
      long size;
    } vector;
    struct {
      struct object *(*fn)(struct object *arguments,
			   struct object *environment);
    } primitive_proc;
    struct {
      struct object *parameters;
      struct object *body;
      struct object *env;
    } compound_proc; /* also syntax */
    struct {
      FILE *stream;
    } input_port;
    struct {
      FILE *stream;
    } output_port;
  } data;

  /* garbage collection data */
  struct object* next;
  char mark;
} object;

typedef struct object* (prim_proc)(struct object*,
				   struct object*);

/* some basic functions for dealing with tagged types */

void *MALLOC(long size);

object *alloc_object(void);

object *make_uninterned_symbol(char *value);

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

object *make_vector(object *fill, long size);

object *list_to_vector(object *list);

char is_vector(object *obj);

/* statistics */
long get_alloc_count();
long get_cons_count();

#define caar(obj) car(car(obj))
#define cadr(obj) car(cdr(obj))
#define caddr(obj) car(cdr(cdr(obj)))

#define first(obj) car(obj)
#define second(obj) cadr(obj)
#define third(obj) caddr(obj)
#define list1(a) cons(a,the_empty_list)
#define list2(a,b) cons(a,list1(b))
#define list3(a,b,c) cons(a,list2(b,c))
#define list4(a,b,c,d) cons(a,list3(b,c,d))
#define list5(a,b,c,d,e) cons(a,list4(b,c,d,e))

#define LONG(x) x->data.fixnum.value
#define CHAR(x) x->data.character.value
#define STRING(x) x->data.string.value
#define BOOLEAN(x) x->data.boolean.value
#define INPUT(x) x->data.input_port.stream
#define OUTPUT(x) x->data.output_port.stream
#define VARRAY(obj) (obj->data.vector.objects)
#define VSIZE(obj) (obj->data.vector.size)

object *make_primitive_proc(prim_proc fn);
char is_primitive_proc(object *obj);
object *make_compound_proc(object *parameters, object *body,
			   object *env);
char is_compound_proc(object *obj);

object *make_syntax_proc(object *parameters, object *body);
char is_syntax_proc(object *obj);

object *make_input_port(FILE *stream);
object *make_output_port(FILE *stream);
char is_input_port(object *obj);
char is_output_port(object *obj);
char is_eof_object(object *obj);

char is_atom(object *obj);

#endif
