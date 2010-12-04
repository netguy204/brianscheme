#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>

#include "types.h"
#include "symbols.h"
#include "read.h"
#include "gc.h"
/*temp*/
#include "interp.h"

object *throw_read(char * msg, ...) {
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
  exit(1);
  return NULL;
}


char is_delimiter(int c) {
  return isspace(c) || c == EOF ||
    c == '(' || c == ')' ||
    c == '"' || c ==';';
}

char is_initial(char c) {
  return isalpha((int)c) || c == '*' || c == '/' ||
    c == '>' || c == '<' || c == '=' || c == '?' ||
    c == '?' || c == '!' || c == '&' || c == '.';
}

int peek(FILE *in) {
  int c;
  c = getc(in);
  ungetc(c, in);
  return c;
}

void eat_whitespace(FILE *in) {
  int c;

  while ((c = getc(in)) != EOF) {
    if(isspace(c)) {
      continue;
    }
    else if(c == ';') {
      /* eat until eol */
      while (((c = getc(in)) != EOF) && (c != '\n')) {
	continue;
      }
      eat_whitespace(in);
      return;
    }
    ungetc(c, in);
    break;
  }
}

void eat_expected_string(FILE *in, char *str) {
  int c;
  while(*str != '\0') {
    c = getc(in);
    if(c != *str) {
      throw_read("unexpected character '%c'\n", c);
    }
    str++;
  }
}

void peek_expected_delimiter(FILE *in) {
  if(!is_delimiter(peek(in))) {
    throw_read("character not followed by delimiter\n");
  }
}

object *read(FILE *in);

object *read_character(FILE *in) {
  int c;

  c = getc(in);
  switch(c) {
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

object *read_pair(FILE *in) {
  int c;
  object *car_obj;
  object *cdr_obj;

  eat_whitespace(in);

  c = getc(in);
  if(c == ')') {
    return the_empty_list;
  }
  ungetc(c, in);

  car_obj = read(in);
  push_root(&car_obj);

  eat_whitespace(in);

  c = getc(in);
  if(c == '.') {
    peek_expected_delimiter(in);

    cdr_obj = read(in);
    push_root(&cdr_obj);

    eat_whitespace(in);
    c = getc(in);
    if(c != ')') {
      return throw_read("improper list missing trailing paren\n");
    }

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);

    return result;
  } else {
    ungetc(c, in);

    cdr_obj = read_pair(in);
    push_root(&cdr_obj);

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);
    return result;
  }
}

object *read(FILE *in) {
  int c;
  short sign = 1;
  int i;
  long num = 0;
#define BUFFER_MAX 1000
  char buffer[BUFFER_MAX];

  eat_whitespace(in);
  c = getc(in);
  if(c == '#') {
    c = getc(in);
    switch(c) {
    case 't':
      return true;
    case 'f':
      return false;
    case '\\':
      return read_character(in);
    default:
      return throw_read("unknown boolean or character literal\n");
    }
  }
  else if(isdigit(c) || (c == '-' && (isdigit(peek(in))))) {
    if(c == '-') {
      sign = -1;
    } else {
      sign = 1;
      ungetc(c, in);
    }

    while(isdigit(c = getc(in))) {
      num = (num * 10) + (c - '0');
    }
    num *= sign;
    if(is_delimiter(c)) {
      ungetc(c, in);
      return make_fixnum(num);
    } else {
      return throw_read("number was not followed by delimiter");
    }
  }
  else if(is_initial(c) ||
	  ((c == '+' || c == '-') &&
	   is_delimiter(peek(in)))) {
    i = 0;
    while(is_initial(c) || isdigit(c) ||
	  c == '+' || c == '-') {
      if(i < BUFFER_MAX - 1) {
	buffer[i++] = c;
      } else {
	return throw_read("symbol exceeded %d chars", BUFFER_MAX);
      }
      c = getc(in);
    }
    if(is_delimiter(c)) {
      buffer[i] = '\0';
      ungetc(c, in);
      return make_symbol(buffer);
    } else {
      return throw_read("symbol not followed by delimiter. found %c\n",
			c);
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
    object *quoted = read(in);
    push_root(&quoted);
    quoted = cons(quoted, the_empty_list);
    quoted = cons(quote_symbol, quoted);
    pop_root(&quoted);
    return quoted;
  }
  else if(c == ',') {
    object *unquoted = read(in);
    push_root(&unquoted);
    unquoted = cons(unquoted, the_empty_list);
    unquoted = cons(unquote_symbol, unquoted);
    pop_root(&unquoted);
    return unquoted;
  }
  else if(c == '`') {
    object *qquoted = read(in);
    push_root(&qquoted);
    qquoted = cons(qquoted, the_empty_list);
    qquoted = cons(quasiquote_symbol, qquoted);
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
