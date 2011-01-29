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

#ifndef NO_READLINE
#include <editline.h>
#endif

#include "types.h"
#include "read.h"
#include "gc.h"
/*temp*/
#include "interp.h"

char *prompt = "> ";

typedef struct {
  FILE *stream;
  char *buffer, *p;
  int ungetc;
  char *history;
} read_buffer;

object *throw_read(char *msg, ...) {
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
  exit(1);
  return NULL;
}

char *xstrdup(char *str) {
  char *newstr = xmalloc(strlen(str) + 1);
  strncpy(newstr, str, strlen(str) + 1);
  return newstr;
}

/* Append the current buffer to the history. */
void grow_history(read_buffer * in) {
  if(in->buffer == NULL || (strlen(in->buffer) == 0))
    return;
  if(in->history == NULL) {
    in->history = xstrdup(in->buffer);
    return;
  }
  size_t histlen = strlen(in->history);
  size_t bufflen = strlen(in->buffer);
  in->history = realloc(in->history, histlen + bufflen + 2);
  in->history[histlen] = '\n';
  strncpy(in->history + histlen + 1, in->buffer, bufflen + 1);
}

/* Wraps calls to getc() to work on our buffer. */
int read_getc(read_buffer * in) {
#ifdef NO_READLINE
  if(true) {
#else
  if(in->stream != stdin) {
#endif
    return getc(in->stream);
  }
  if(in->ungetc != -1) {
    char c = in->ungetc;
    in->ungetc = -1;
    return c;
  }
  if(in->p != NULL && *in->p == '\0') {
    in->p = NULL;
    return '\n';
  }
  if(in->p == NULL) {
    free(in->buffer);
#ifndef NO_READLINE
    in->buffer = in->p = readline(in->history == NULL ? prompt : "");
#endif
    grow_history(in);
    if(in->buffer == NULL)
      return EOF;
    return read_getc(in);
  }
  return *in->p++;
}

/* Wraps calls to ungetc() to work on our buffer. */
void read_ungetc(char c, read_buffer * in) {
#ifdef NO_READLINE
  if(true) {
#else
  if(in->stream != stdin) {
#endif
    ungetc(c, in->stream);
  }
  else {
    in->ungetc = c;
  }
}

char is_delimiter(int c) {
  return isspace(c) || c == EOF ||
    c == '(' || c == ')' || c == '"' || c == ';';
}

char is_initial(char c) {
  return isalpha((int)c) || c == '*' || c == '/' ||
    c == '>' || c == '<' || c == '=' || c == '?' ||
    c == '?' || c == '!' || c == '&' || c == '.' || c == ':' || c == '%';
}

int peek(read_buffer * in) {
  int c;
  c = read_getc(in);
  read_ungetc(c, in);
  return c;
}

int eat_whitespace(read_buffer * in) {
  int c;

  while((c = read_getc(in)) != EOF) {
    if(isspace(c)) {
      continue;
    }
    else if(c == ';') {
      /* eat until eol */
      while(((c = read_getc(in)) != EOF) && (c != '\n')) {
	continue;
      }
      if(c == EOF)
	return c;
      return eat_whitespace(in);
    }
    read_ungetc(c, in);
    break;
  }
  return c;
}

void eat_expected_string(read_buffer * in, char *str) {
  int c;
  while(*str != '\0') {
    c = read_getc(in);
    if(c != *str) {
      throw_read("unexpected character '%c'\n", c);
    }
    str++;
  }
}

void peek_expected_delimiter(read_buffer * in) {
  if(!is_delimiter(peek(in))) {
    throw_read("character not followed by delimiter\n");
  }
}

object *lisp_read(read_buffer * in);

object *read_character(read_buffer * in) {
  int c;

  c = read_getc(in);
  switch (c) {
  case EOF:
    return throw_read("incomplete character literal\n");
  case 's':
    if(peek(in) == 'p') {
      eat_expected_string(in, "pace");
      peek_expected_delimiter(in);
      return make_character(' ');
    }
    break;
  case 'n':
    if(peek(in) == 'e') {
      eat_expected_string(in, "ewline");
      peek_expected_delimiter(in);
      return make_character('\n');
    }
    break;
  }
  peek_expected_delimiter(in);
  return make_character(c);
}

object *read_pair(read_buffer * in) {
  int c;
  object *car_obj;
  object *cdr_obj;

  if(eat_whitespace(in) == EOF)
    return throw_read("unexpected EOF\n");

  c = read_getc(in);
  if(c == ')') {
    return g->empty_list;
  }
  read_ungetc(c, in);

  car_obj = lisp_read(in);
  push_root(&car_obj);

  if(eat_whitespace(in) == EOF)
    return throw_read("unexpected EOF\n");

  c = read_getc(in);
  if(c == '.') {
    peek_expected_delimiter(in);

    cdr_obj = lisp_read(in);
    push_root(&cdr_obj);

    if(eat_whitespace(in) == EOF)
      return throw_read("unexpected EOF\n");
    c = read_getc(in);
    if(c != ')') {
      return throw_read("improper list missing trailing paren\n");
    }

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);

    return result;
  }
  else {
    read_ungetc(c, in);

    cdr_obj = read_pair(in);
    push_root(&cdr_obj);

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);
    return result;
  }
}

object *read_vector(read_buffer * in) {
  int c;
  object *list_head;
  object *list_tail;
  object *current;

  if(eat_whitespace(in) == EOF)
    return throw_read("unexpected EOF\n");

  c = read_getc(in);
  if(c == ')') {
    return g->empty_vector;
  }
  read_ungetc(c, in);

  current = cons(lisp_read(in), g->empty_list);
  push_root(&current);
  list_head = current;
  push_root(&list_head);

  /* this is protected by the head */
  list_tail = list_head;

  if(eat_whitespace(in) == EOF)
    return throw_read("unexpected EOF\n");
  c = read_getc(in);
  while(c != ')') {
    read_ungetc(c, in);

    current = lisp_read(in);
    set_cdr(list_tail, cons(current, g->empty_list));
    list_tail = cdr(list_tail);

    if(eat_whitespace(in) == EOF)
      return throw_read("unexpected EOF\n");
    c = read_getc(in);
  }

  object *vector = list_to_vector(list_head);
  pop_root(&list_head);
  pop_root(&current);

  return vector;
}

object *obj_read(FILE * in) {
  read_buffer r;
  r.stream = in;
  r.buffer = r.p = r.history = NULL;
  r.ungetc = -1;
  object *obj = lisp_read(&r);
  free(r.buffer);
#ifndef NO_READLINE
  add_history(r.history);
#endif
  return obj;
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
	return throw_read("number contained multiple decimal points\n");
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

object *read_number(char c, read_buffer * in) {
  char buffer[128];
  int idx = 0;

  while(idx < 128 && (c == '-' || c == '.' || isdigit(c))) {
    if(idx == 127) {
      return throw_read("too many digits in number");
    }

    buffer[idx++] = c;
    c = read_getc(in);
  }

  if(is_delimiter(c)) {
    read_ungetc(c, in);
  }
  else {
    return throw_read("number was not followed by delimiter");
  }

  buffer[idx] = '\0';

  return string_to_number(buffer);
}

object *lisp_read(read_buffer * in) {
  int c;
  int i;
#define BUFFER_MAX 1000
  char buffer[BUFFER_MAX];

  if(eat_whitespace(in) == EOF)
    return NULL;
  c = read_getc(in);
  if(c == '#') {
    c = read_getc(in);
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
      while(((c = read_getc(in)) != EOF) && (c != '\n'))
	continue;
      return g->false;
    default:
      return throw_read("unknown boolean or character literal '%c' \n", c);
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
	return throw_read("symbol exceeded %d chars", BUFFER_MAX);
      }
      c = read_getc(in);
      if(c == EOF) {
	return throw_read("unexpected EOF\n");
      }
    }
    if(is_delimiter(c)) {
      buffer[i] = '\0';
      read_ungetc(c, in);
      return make_symbol(buffer);
    }
    else {
      return throw_read("symbol not followed by delimiter. found %c\n", c);
    }
  }
  else if(c == '"') {
    i = 0;
    while((c = read_getc(in)) != '"') {
      if(c == '\\') {
	c = read_getc(in);
	if(c == 'n') {
	  c = '\n';
	}
	else if(c == '"') {
	  c = '"';
	}
      }
      if(c == EOF) {
	return throw_read("string literal not terminated\n");
      }
      if(i < BUFFER_MAX - 1) {
	buffer[i++] = c;
      }
      else {
	return throw_read("string exceeded buffer length %d", BUFFER_MAX);
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
    push_root(&quoted);
    quoted = cons(quoted, g->empty_list);
    quoted = cons(g->quote_symbol, quoted);
    pop_root(&quoted);
    return quoted;
  }
  else if(c == ',') {
    char next_char = read_getc(in);
    object *qsym = g->unquote_symbol;

    if(next_char == '@') {
      qsym = g->unquotesplicing_symbol;
    }
    else {
      read_ungetc(next_char, in);
    }

    object *unquoted = lisp_read(in);
    push_root(&unquoted);
    unquoted = cons(unquoted, g->empty_list);
    unquoted = cons(qsym, unquoted);
    pop_root(&unquoted);
    return unquoted;
  }
  else if(c == '`') {
    object *qquoted = lisp_read(in);
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
    return throw_read("bad input. Unexpected '%c'\n", c);
  }
}
