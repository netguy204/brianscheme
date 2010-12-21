#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "types.h"

/* global primitive symbols */

object *the_empty_list;
object *the_empty_vector;
object *false;
object *true;
object *symbol_table;
object *quote_symbol;
object *quasiquote_symbol;
object *unquote_symbol;
object *set_symbol;
object *if_symbol;
object *begin_symbol;
object *lambda_symbol;
object *macro_symbol;
object *debug_symbol;
object *stdin_symbol;
object *stdout_symbol;
object *stderr_symbol;
object *eof_object;
object *exit_hook_symbol;

object *the_empty_environment;
object *the_global_environment;

object *the_call_stack;

#endif
