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

#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>

#include "types.h"
#include "read.h"
#include "gc.h"
/*temp*/
#include "interp.h"

char is_delimiter(int c) {
  return isspace(c) || c == EOF ||
    c == '(' || c == ')' || c == '"' || c == ';';
}

char is_initial(char c) {
  return isalpha((int)c) || c == '*' || c == '/' ||
    c == '>' || c == '<' || c == '=' || c == '?' ||
    c == '?' || c == '!' || c == '&' || c == '.' || c == ':' || c == '%';
}

int peek(FILE * in) {
  int c = getc(in);
  ungetc(c, in);
  return c;
}

int eat_whitespace(FILE * in) {
  int c;

  while((c = getc(in)) != EOF) {
    if(isspace(c)) {
      continue;
    }
    else if(c == ';') {
      /* eat until eol */
      while(((c = getc(in)) != EOF) && (c != '\n')) {
	continue;
      }
      if(c == EOF)
	return c;
      return eat_whitespace(in);
    }
    ungetc(c, in);
    break;
  }
  return c;
}

object *eat_expected_string(FILE * in, char *str) {
  int c;
  while(*str != '\0') {
    c = getc(in);
    if(c != *str) {
      return throw_message("unexpected character '%c'", c);
    }
    str++;
  }
  return g->true;
}

object *peek_expected_delimiter(FILE * in) {
  if(!is_delimiter(peek(in))) {
    return throw_message("character not followed by delimiter");
  }
  return g->true;
}

object *lisp_read(FILE * in);

object *read_character(FILE * in) {
  int c;

  c = getc(in);
  switch (c) {
  case EOF:
    return throw_message("incomplete character literal");
  case 's':
    if(peek(in) == 'p') {
      object *result = eat_expected_string(in, "pace");
      if(is_primitive_exception(result)) return result;

      result = peek_expected_delimiter(in);
      if(is_primitive_exception(result)) return result;

      return make_character(' ');
    }
    break;
  case 'n':
    if(peek(in) == 'e') {
      object *result = eat_expected_string(in, "ewline");
      if(is_primitive_exception(result)) return result;

      result = peek_expected_delimiter(in);
      if(is_primitive_exception(result)) return result;

      return make_character('\n');
    }
  case 't':
    if(peek(in) == 'a') {
      object *result = eat_expected_string(in, "ab");
      if(is_primitive_exception(result)) return result;

      result = peek_expected_delimiter(in);
      if(is_primitive_exception(result)) return result;

      return make_character('\t');
    }
    break;
  }
  object *result = peek_expected_delimiter(in);
  if(is_primitive_exception(result)) return result;

  return make_character(c);
}

object *read_pair(FILE * in) {
  int c;
  object *car_obj;
  object *cdr_obj;

  if(eat_whitespace(in) == EOF)
    return throw_message("unexpected EOF");

  c = getc(in);
  if(c == ')') {
    return g->empty_list;
  }
  ungetc(c, in);

  car_obj = lisp_read(in);
  if(is_primitive_exception(car_obj)) return car_obj;

  push_root(&car_obj);

  if(eat_whitespace(in) == EOF)
    return throw_message("unexpected EOF");

  c = getc(in);
  if(c == '.') {
    object *temp = peek_expected_delimiter(in);
    if(is_primitive_exception(temp)) return temp;

    cdr_obj = lisp_read(in);
    if(is_primitive_exception(cdr_obj)) return cdr_obj;

    push_root(&cdr_obj);

    if(eat_whitespace(in) == EOF)
      return throw_message("unexpected EOF");
    c = getc(in);
    if(c != ')') {
      return throw_message("improper list missing trailing paren");
    }

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);

    return result;
  }
  else {
    ungetc(c, in);

    cdr_obj = read_pair(in);
    if(is_primitive_exception(cdr_obj)) return cdr_obj;

    push_root(&cdr_obj);

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);
    return result;
  }
}

object *read_vector(FILE * in) {
  int c;
  object *list_head;
  object *list_tail;
  object *current;

  if(eat_whitespace(in) == EOF)
    return throw_message("unexpected EOF");

  c = getc(in);
  if(c == ')') {
    return g->empty_vector;
  }
  ungetc(c, in);

  object *val = lisp_read(in);
  if(is_primitive_exception(val)) return val;

  current = cons(val, g->empty_list);
  push_root(&current);
  list_head = current;
  push_root(&list_head);

  /* this is protected by the head */
  list_tail = list_head;

  if(eat_whitespace(in) == EOF)
    return throw_message("unexpected EOF");
  c = getc(in);
  while(c != ')') {
    ungetc(c, in);

    current = lisp_read(in);
    if(is_primitive_exception(current)) return current;

    set_cdr(list_tail, cons(current, g->empty_list));
    list_tail = cdr(list_tail);

    if(eat_whitespace(in) == EOF)
      return throw_message("unexpected EOF");
    c = getc(in);
  }

  object *vector = list_to_vector(list_head);
  pop_root(&list_head);
  pop_root(&current);

  return vector;
}

object *obj_read(FILE * in) {
  return lisp_read(in);
}

object *string_to_number(char *buffer) {
  char *iter = buffer;
  char floatnum = 0;
  char c;

  while(*iter) {
    c = *iter;
    ++iter;

    if(c == '.') {
      if(!floatnum) {
	floatnum = 1;
      }
      else {
	return throw_message("number contained multiple decimal points");
      }
    }
  }

  if(floatnum) {
    return make_real(atof(buffer));
  }
  else {
    return make_fixnum(atol(buffer));
  }
}

object *read_number(char c, FILE * in) {
  char buffer[128];
  int idx = 0;

  while(idx < 128 && (c == '-' || c == '.' || isdigit(c))) {
    if(idx == 127) {
      return throw_message("too many digits in number");
    }

    buffer[idx++] = c;
    c = getc(in);
  }

  if(is_delimiter(c)) {
    ungetc(c, in);
  }
  else {
    return throw_message("number was not followed by delimiter");
  }

  buffer[idx] = '\0';

  return string_to_number(buffer);
}

object *lisp_read(FILE * in) {
  int c;
  int i;
#define BUFFER_MAX 1000
  char buffer[BUFFER_MAX];

  if(eat_whitespace(in) == EOF)
    return NULL;
  c = getc(in);
  if(c == '#') {
    c = getc(in);
    switch (c) {
    case 't':
      return g->true;
    case 'f':
      return g->false;
    case '\\':
      return read_character(in);
    case '(':
      return read_vector(in);
    case '!':
      /* Treat like a comment. */
      while(((c = getc(in)) != EOF) && (c != '\n'))
	continue;
      return g->false;
    default:
      return throw_message("unknown boolean or character literal '%c' \n", c);
    }
  }
  else if(isdigit(c) || (c == '-' && (isdigit(peek(in))))) {
    return read_number(c, in);
  }
  else if(is_initial(c) || ((c == '+' || c == '-') && is_delimiter(peek(in)))) {
    i = 0;
    while(is_initial(c) || isdigit(c) || c == '+' || c == '-') {
      if(i < BUFFER_MAX - 1) {
	buffer[i++] = c;
      }
      else {
	return throw_message("symbol exceeded %d chars", BUFFER_MAX);
      }
      c = getc(in);
      if(c == EOF) {
	return throw_message("unexpected EOF");
      }
    }
    if(is_delimiter(c)) {
      buffer[i] = '\0';
      ungetc(c, in);
      return make_symbol(buffer);
    }
    else {
      return throw_message("symbol not followed by delimiter. found %c", c);
    }
  }
  else if(c == '"') {
    i = 0;
    while((c = getc(in)) != '"') {
      if(c == '\\') {
	c = getc(in);
	if(c == 'n') {
	  c = '\n';
	}
	else if(c == '"') {
	  c = '"';
	}
	else if(c == 't') {
	  c = '\t';
	}
      }
      if(c == EOF) {
	return throw_message("string literal not terminated");
      }
      if(i < BUFFER_MAX - 1) {
	buffer[i++] = c;
      }
      else {
	return throw_message("string exceeded buffer length %d", BUFFER_MAX);
      }
    }
    buffer[i] = '\0';
    return make_string(buffer);
  }
  else if(c == '(') {
    return read_pair(in);
  }
  else if(c == '\'') {
    object *quoted = lisp_read(in);
    if(is_primitive_exception(quoted)) return quoted;

    push_root(&quoted);
    quoted = cons(quoted, g->empty_list);
    quoted = cons(g->quote_symbol, quoted);
    pop_root(&quoted);
    return quoted;
  }
  else if(c == ',') {
    char next_char = getc(in);
    object *qsym = g->unquote_symbol;

    if(next_char == '@') {
      qsym = g->unquotesplicing_symbol;
    }
    else {
      ungetc(next_char, in);
    }

    object *unquoted = lisp_read(in);
    if(is_primitive_exception(unquoted)) return unquoted;

    push_root(&unquoted);
    unquoted = cons(unquoted, g->empty_list);
    unquoted = cons(qsym, unquoted);
    pop_root(&unquoted);
    return unquoted;
  }
  else if(c == '`') {
    object *qquoted = lisp_read(in);
    if(is_primitive_exception(qquoted)) return qquoted;

    push_root(&qquoted);
    qquoted = cons(qquoted, g->empty_list);
    qquoted = cons(g->quasiquote_symbol, qquoted);
    pop_root(&qquoted);
    return qquoted;
  }
  else if(c == EOF) {
    return NULL;
  }
  else {
    return throw_message("bad input. Unexpected '%c'", c);
  }
}
