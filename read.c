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
    c == '>' || c == '<' || c == '=' || c == '?' || c == '_' ||
    c == '?' || c == '!' || c == '&' || c == '.' || c == ':' || c == '%';
}

int eat_whitespace(stream_reader * in) {
  int c;

  while((c = read_stream(in)) != EOF) {
    if(isspace(c)) {
      continue;
    }
    else if(c == ';') {
      /* eat until eol */
      while(((c = read_stream(in)) != EOF) && (c != '\n')) {
	continue;
      }
      if(c == EOF)
	return c;
      return eat_whitespace(in);
    }
    unread_stream(in, c);
    break;
  }
  return c;
}

object *eat_expected_string(stream_reader * in, char *str) {
  int c;
  while(*str != '\0') {
    c = read_stream(in);
    if(c != *str) {
      return throw_message("unexpected character '%c'", c);
    }
    str++;
  }
  return g->true;
}

object *peek_expected_delimiter(stream_reader * in) {
  if(!is_delimiter(peek_stream(in))) {
    return throw_message("character not followed by delimiter");
  }
  return g->true;
}

object *lisp_read(stream_reader * in);

object *read_character(stream_reader * in) {
  int c;

  c = read_stream(in);
  switch (c) {
  case EOF:
    return throw_message("incomplete character literal");
  case 's':
    if(peek_stream(in) == 'p') {
      object *result = eat_expected_string(in, "pace");
      if(is_primitive_exception(result))
	return result;

      result = peek_expected_delimiter(in);
      if(is_primitive_exception(result))
	return result;

      return make_character(' ');
    }
    break;
  case 'n':
    if(peek_stream(in) == 'e') {
      object *result = eat_expected_string(in, "ewline");
      if(is_primitive_exception(result))
	return result;

      result = peek_expected_delimiter(in);
      if(is_primitive_exception(result))
	return result;

      return make_character('\n');
    }
  case 't':
    if(peek_stream(in) == 'a') {
      object *result = eat_expected_string(in, "ab");
      if(is_primitive_exception(result))
	return result;

      result = peek_expected_delimiter(in);
      if(is_primitive_exception(result))
	return result;

      return make_character('\t');
    }
    break;
  }
  object *result = peek_expected_delimiter(in);
  if(is_primitive_exception(result))
    return result;

  return make_character(c);
}

object *read_pair(stream_reader * in) {
  int c;
  object *car_obj;
  object *cdr_obj;

  if(eat_whitespace(in) == EOF)
    return throw_message("unexpected EOF");

  c = read_stream(in);
  if(c == ')') {
    return g->empty_list;
  }
  unread_stream(in, c);

  car_obj = lisp_read(in);
  if(is_primitive_exception(car_obj))
    return car_obj;

  push_root(&car_obj);

  if(eat_whitespace(in) == EOF)
    return throw_message("unexpected EOF");

  c = read_stream(in);
  if(c == '.') {
    object *temp = peek_expected_delimiter(in);
    if(is_primitive_exception(temp))
      return temp;

    cdr_obj = lisp_read(in);
    if(is_primitive_exception(cdr_obj))
      return cdr_obj;

    push_root(&cdr_obj);

    if(eat_whitespace(in) == EOF)
      return throw_message("unexpected EOF");
    c = read_stream(in);
    if(c != ')') {
      return throw_message("improper list missing trailing paren");
    }

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);

    return result;
  }
  else {
    unread_stream(in, c);

    cdr_obj = read_pair(in);
    if(is_primitive_exception(cdr_obj))
      return cdr_obj;

    push_root(&cdr_obj);

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);
    return result;
  }
}

object *read_vector(stream_reader * in) {
  int c;
  object *list_head;
  object *list_tail;
  object *current;

  if(eat_whitespace(in) == EOF)
    return throw_message("unexpected EOF");

  c = read_stream(in);
  if(c == ')') {
    return g->empty_vector;
  }
  unread_stream(in, c);

  object *val = lisp_read(in);
  if(is_primitive_exception(val))
    return val;

  current = cons(val, g->empty_list);
  push_root(&current);
  list_head = current;
  push_root(&list_head);

  /* this is protected by the head */
  list_tail = list_head;

  if(eat_whitespace(in) == EOF)
    return throw_message("unexpected EOF");
  c = read_stream(in);
  while(c != ')') {
    unread_stream(in, c);

    current = lisp_read(in);
    if(is_primitive_exception(current))
      return current;

    set_cdr(list_tail, cons(current, g->empty_list));
    list_tail = cdr(list_tail);

    if(eat_whitespace(in) == EOF)
      return throw_message("unexpected EOF");
    c = read_stream(in);
  }

  object *vector = list_to_vector(list_head);
  pop_root(&list_head);
  pop_root(&current);

  return vector;
}

object *obj_read(stream_reader * in) {
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

object *read_number(char c, stream_reader * in) {
  char buffer[128];
  int idx = 0;

  while(idx < 128 && (c == '-' || c == '.' || isdigit(c))) {
    if(idx == 127) {
      return throw_message("too many digits in number");
    }

    buffer[idx++] = c;
    c = read_stream(in);
  }

  if(is_delimiter(c)) {
    unread_stream(in, c);
  }
  else {
    return throw_message("number was not followed by delimiter");
  }

  buffer[idx] = '\0';

  return string_to_number(buffer);
}

object *lisp_read(stream_reader * in) {
  int c;
  int i;
#define BUFFER_MAX 1000
  char buffer[BUFFER_MAX];

  if(eat_whitespace(in) == EOF)
    return NULL;
  c = read_stream(in);
  if(c == '#') {
    c = read_stream(in);
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
      while(((c = read_stream(in)) != EOF) && (c != '\n'))
	continue;
      return g->false;
    default:
      return throw_message("unknown boolean or character literal '%c' \n", c);
    }
  }
  else if(isdigit(c) || (c == '-' && (isdigit(peek_stream(in))))) {
    return read_number(c, in);
  }
  else if(is_initial(c)
	  || ((c == '+' || c == '-') && is_delimiter(peek_stream(in)))) {
    i = 0;
    while(is_initial(c) || isdigit(c) || c == '+' || c == '-') {
      if(i < BUFFER_MAX - 1) {
	buffer[i++] = c;
      }
      else {
	return throw_message("symbol exceeded %d chars", BUFFER_MAX);
      }
      c = read_stream(in);
      if(c == EOF) {
	return throw_message("unexpected EOF");
      }
    }
    if(is_delimiter(c)) {
      buffer[i] = '\0';
      unread_stream(in, c);
      return make_symbol(buffer);
    }
    else {
      return throw_message("symbol not followed by delimiter. found %c", c);
    }
  }
  else if(c == '"') {
    i = 0;
    while((c = read_stream(in)) != '"') {
      if(c == '\\') {
	c = read_stream(in);
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
    if(is_primitive_exception(quoted))
      return quoted;

    push_root(&quoted);
    quoted = cons(quoted, g->empty_list);
    quoted = cons(g->quote_symbol, quoted);
    pop_root(&quoted);
    return quoted;
  }
  else if(c == ',') {
    char next_char = read_stream(in);
    object *qsym = g->unquote_symbol;

    if(next_char == '@') {
      qsym = g->unquotesplicing_symbol;
    }
    else {
      unread_stream(in, next_char);
    }

    object *unquoted = lisp_read(in);
    if(is_primitive_exception(unquoted))
      return unquoted;

    push_root(&unquoted);
    unquoted = cons(unquoted, g->empty_list);
    unquoted = cons(qsym, unquoted);
    pop_root(&unquoted);
    return unquoted;
  }
  else if(c == '`') {
    object *qquoted = lisp_read(in);
    if(is_primitive_exception(qquoted))
      return qquoted;

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

int read_file_stream(stream_reader * reader) {
  file_stream_reader *file_reader = (file_stream_reader *) reader;
  return getc(file_reader->source);
}

void unread_file_stream(stream_reader * reader, int last_read) {
  file_stream_reader *file_reader = (file_stream_reader *) reader;
  ungetc(last_read, file_reader->source);
}

int peek_file_stream(stream_reader * in) {
  int c = read_stream(in);
  unread_stream(in, c);
  return c;
}

void release_file_stream(stream_reader * reader) {
  file_stream_reader *file_reader = (file_stream_reader *) reader;
  fclose(file_reader->source);
  free(file_reader);
}

void release_popen_reader(stream_reader * reader) {
  file_stream_reader *file_reader = (file_stream_reader *) reader;
  pclose(file_reader->source);
}

stream_reader *make_file_reader(FILE * file) {
  file_stream_reader *reader = malloc(sizeof(file_stream_reader));
  reader->reader.reader = &read_file_stream;
  reader->reader.unreader = &unread_file_stream;
  reader->reader.peeker = &peek_file_stream;
  reader->reader.releaser = &release_file_stream;
  reader->source = file;
  return (stream_reader *) reader;
}

stream_reader *make_popen_reader(const char * cmd) {
  stream_reader *reader = make_file_reader(popen(cmd, "r"));
  file_stream_reader *file_reader = (file_stream_reader *) reader;
  file_reader->reader.releaser = &release_popen_reader;
  return reader;
}

int read_string_stream(stream_reader * reader) {
  string_stream_reader *string_reader = (string_stream_reader *) reader;
  char value = string_reader->source[string_reader->position++];
  if(value == 0)
    return EOF;
  return value;
}

void unread_string_stream(stream_reader * reader, int last_read) {
  string_stream_reader *string_reader = (string_stream_reader *) reader;
  string_reader->position--;
}

int peek_string_stream(stream_reader * reader) {
  string_stream_reader *string_reader = (string_stream_reader *) reader;
  char value = string_reader->source[string_reader->position];
  if(value == 0)
    return EOF;
  return value;
}

void release_string_stream(stream_reader * reader) {
  string_stream_reader *string_reader = (string_stream_reader *) reader;
  free(string_reader->source);
  free(string_reader);
}

stream_reader *make_string_reader(const char *string) {
  string_stream_reader *reader = malloc(sizeof(string_stream_reader));
  reader->reader.reader = &read_string_stream;
  reader->reader.unreader = &unread_string_stream;
  reader->reader.peeker = &peek_string_stream;
  reader->reader.releaser = &release_string_stream;
  reader->source = malloc(sizeof(char) * (strlen(string) + 1));
  strcpy(reader->source, string);
  reader->position = 0;
  return (stream_reader *) reader;
}

int read_stream(stream_reader * stream) {
  return stream->reader(stream);
}

void unread_stream(stream_reader * stream, int last_read) {
  stream->unreader(stream, last_read);
}

int peek_stream(stream_reader * stream) {
  return stream->peeker(stream);
}

void release_stream_reader(stream_reader * stream) {
  stream->releaser(stream);
}

void write_file_stream(stream_writer * writer, char datum) {
  file_stream_writer *file_writer = (file_stream_writer *) writer;
  putc(datum, file_writer->destination);
}

void close_file_stream(stream_writer * writer) {
  file_stream_writer *file_writer = (file_stream_writer *) writer;
  fclose(file_writer->destination);
}

void close_popen_writer(stream_writer * writer) {
  file_stream_writer *file_writer = (file_stream_writer *) writer;
  pclose(file_writer->destination);
}

stream_writer *make_file_writer(FILE * file) {
  file_stream_writer *writer = malloc(sizeof(file_stream_writer));
  writer->destination = file;
  writer->writer.writer = &write_file_stream;
  writer->writer.releaser = &close_file_stream;
  return (stream_writer *) writer;
}

stream_writer *make_popen_writer(const char * cmd) {
  stream_writer *writer = make_file_writer(popen(cmd, "w"));
  file_stream_writer *file_writer = (file_stream_writer *) writer;
  file_writer->writer.releaser = &close_popen_writer;
  return writer;
}

void write_string_stream(stream_writer * writer, char datum) {
  string_stream_writer *string_writer = (string_stream_writer *) writer;
  if(string_writer->position + 1 < string_writer->destination_capacity) {
    // prepadding the null means we can read this buffer in a separate thread without locking
    string_writer->destination[string_writer->position + 1] = '\0';
    string_writer->destination[string_writer->position++] = datum;
  }
}

void close_string_stream(stream_writer * writer) {
  // all memory is external, do nothing
}

stream_writer *make_string_writer(char *buffer, int length) {
  string_stream_writer *writer = malloc(sizeof(string_stream_writer));
  writer->writer.writer = &write_string_stream;
  writer->writer.releaser = &close_string_stream;
  writer->destination = buffer;
  writer->destination_capacity = length;
  writer->position = 0;
  return (stream_writer *) writer;
}

void write_stream(stream_writer * writer, char datum) {
  writer->writer(writer, datum);
}

void release_stream_writer(stream_writer * writer) {
  writer->releaser(writer);
}

void stream_fprintf(stream_writer * writer, char *msg, ...) {
  char buffer[1024];
  char *ptr;

  va_list args;
  va_start(args, msg);
  int error = vsnprintf(buffer, 1024, msg, args);
  va_end(args);

  if(error < 0)
    return;

  for(ptr = buffer; *ptr != 0; ++ptr) {
    write_stream(writer, *ptr);
  }
}
