#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "types.h"

/* global primitive symbols */

extern object *the_empty_list;
extern object *the_empty_vector;
extern object *false;
extern object *true;
extern object *symbol_table;
extern object *quote_symbol;
extern object *quasiquote_symbol;
extern object *unquote_symbol;
extern object *unquotesplicing_symbol;
extern object *set_symbol;
extern object *if_symbol;
extern object *begin_symbol;
extern object *lambda_symbol;
extern object *macro_symbol;
extern object *stdin_symbol;
extern object *stdout_symbol;
extern object *stderr_symbol;
extern object *eof_object;
extern object *exit_hook_symbol;

extern object *the_empty_environment;
extern object *the_global_environment;
extern object *vm_global_environment;

#endif
