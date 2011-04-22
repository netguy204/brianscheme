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

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#include "vm.h"
#include "types.h"
#include "interp.h"
#include "read.h"
#include "gc.h"
#include "ffi.h"

char profiling_enabled;

#define ARG1 ((long)(codes[pc-2]))
#define ARG2 ((long)(codes[pc-1]))
#define BC void*

#define length1(x) (cdr(x) == the_empty_list)

#define opcode_table(define)			\
  define(pushvarargs)				\
  define(chainframe)				\
  define(endframe)				\
  define(spush)					\
  define(sset)					\
  define(swap)					\
  define(return)				\
  define(const)					\
  define(fn)					\
  define(fjump)					\
  define(tjump)					\
  define(jump)					\
  define(callj)					\
  define(lvar)					\
  define(save)					\
  define(gvar)					\
  define(lset)					\
  define(gset)					\
  define(setcc)					\
  define(cc)					\
  define(incprof)				\
  define(pop)

/* generate the symbol variable declarations */
#define generate_decls(opcode) object * opcode ## _op;
opcode_table(generate_decls)

/* generate an enumeration of all of the opcodes */
#define generate_enum(opcode) _ ## opcode ## _,
     enum {
       opcode_table(generate_enum)
	 INVALID_BYTECODE,
     } opcodes;

/* generate the stringified form */
#define generate_string(opcode) "" # opcode,

     const char *bytecode_str[] = {
       opcode_table(generate_string)
     };

/* generate a function that converts a symbol into the corresponding
   bytecode */
#define generate_sym_to_code(opcode)			\
  if(sym == opcode ## _op) {				\
    return make_alien( (void*)_ ## opcode ## _, NULL);	\
  }

object *symbol_to_code(object * sym) {
  opcode_table(generate_sym_to_code);
  return g->false;
}

DEFUN1(symbol_to_code_proc) {
  return symbol_to_code(FIRST);
}

DEFUN1(make_bytecode_array_proc) {
  BC *bca = MALLOC(LONG(FIRST) * sizeof(BC));
  return make_alien(bca, g->free_ptr_fn);
}

DEFUN1(get_bytecode_proc) {
  BC *bca = ALIEN_PTR(FIRST);
  return make_alien(bca[LONG(SECOND)], NULL);
}

DEFUN1(set_bytecode_proc) {
  BC *bca = ALIEN_PTR(FIRST);
  if(is_alien(THIRD)) {
    bca[LONG(SECOND)] = (BC) ALIEN_PTR(THIRD);
  } else if(is_fixnum(THIRD)) {
    bca[LONG(SECOND)] = (BC) LONG(THIRD);
  } else {
    bca[LONG(SECOND)] = (BC) -1;
  }
  return THIRD;
}

/* generate a function that converts a bytecode back into its
   corresponding symbol */

#define generate_code_to_sym(opcode)			\
  if(CHAR(FIRST) == _ ## opcode ## _) {			\
    return make_symbol("" # opcode);			\
  }

DEFUN1(code_to_symbol_proc) {
  opcode_table(generate_code_to_sym)
    return g->false;
}

#define VM_ERROR_RESTART(obj)			\
  do {						\
    VPUSH(obj, stack, stack_top);		\
    fn = g->vm_error_restart;			\
    pc = 0;					\
    n_args = 1;					\
    env = CENV(g->vm_error_restart);		\
    top = make_vector(g->empty_list, 2);	\
    env = cons(top, env);			\
    goto vm_fn_begin;				\
  } while(0)

#define VM_ASSERT(test, msg, ...)			\
  do {							\
    if(!(test)) {					\
      char * buffer = MALLOC(1024);			\
      snprintf(buffer, 1024, msg, ##__VA_ARGS__);	\
      object *msg_obj = make_string(buffer);		\
      FREE(buffer);					\
      VM_ERROR_RESTART(msg_obj);			\
    }							\
  } while(0)

void vector_push(object * stack, object * thing, long top) {
  if(top == VSIZE(stack)) {
    long old_size = VSIZE(stack);
    VSIZE(stack) = old_size * 1.8;
    VARRAY(stack) = REALLOC(VARRAY(stack), sizeof(object *)
			    * VSIZE(stack));
    int ii;
    for(ii = old_size; ii < VSIZE(stack); ++ii) {
      VARRAY(stack)[ii] = g->empty_list;
    }
  }
  VARRAY(stack)[top++] = thing;
}

object *vector_pop(object * stack, long top) {
  object *old = VARRAY(stack)[--top];
  VARRAY(stack)[top] = g->empty_list;
  return old;
}

char sigint_set = 0;

void vm_sigint_handler(int arg __attribute__ ((unused))) {
  sigint_set = 1;
}

#define VM_RETURN(obj)				\
  do {						\
    pop_root(&top);				\
    pop_root(&env);				\
    pop_root(&fn);				\
    pop_root(&stack);				\
    return obj;					\
  } while(0)

/** this code needs to be spliced into several
 *  locations so we just define it once here
 */
#define RETURN_OPCODE_INSTRUCTIONS			\
  /* if there's only one value on the stack,		\
   * we're done */                                      \
  if(stack_top == initial_top + 1) {			\
    object *val;					\
    VPOP(val, stack, stack_top);			\
    VM_RETURN(val);					\
  } else {						\
    object *retval;					\
    object *val;					\
    VPOP(retval, stack, stack_top);			\
    VPOP(val, stack, stack_top);			\
    fn_first_arg = SMALL_FIXNUM(val);			\
    VPOP(val, stack, stack_top);			\
    pc = SMALL_FIXNUM(val);				\
    VPOP(fn, stack, stack_top);				\
    VPOP(env, stack, stack_top);			\
    /* setup for the next loop */			\
    VPUSH(retval, stack, stack_top);			\
    goto vm_fn_begin;					\
  }

#ifdef VM_DEBUGGING
#define VM_DEBUG(msg, obj)			\
  do {						\
    fprintf(stdout, "%s: ", msg);		\
    owrite(stdout, obj);			\
    fprintf(stdout, "\n");			\
  } while(0)
#else
#define VM_DEBUG(msg, obj)
#endif

object *vm_execute(object * fn, object * stack, long stack_top, long n_args) {
  object *const_array;

  object *env;

  unsigned char opcode;
  object *top;

  long initial_top = stack_top - n_args;
  long fn_first_arg = stack_top - n_args;
  long pc = 0;

  /* bootstrap an empty frame for this function since the callj opcode
     won't have built one for us */

  env = CENV(fn);
  top = g->empty_list;

  push_root(&stack);
  push_root(&fn);
  push_root(&env);
  push_root(&top);

vm_fn_begin:
  if(!is_compiled_proc(fn) && !is_compiled_syntax_proc(fn)) {
    owrite(stderr, fn);
    return g->false;
  }

  long num_codes = LONG(car(BYTECODE(fn)));
  BC *codes = ALIEN_PTR(cadr(BYTECODE(fn)));
  const_array = caddr(BYTECODE(fn));

  VM_DEBUG("stack", stack);

vm_begin:
  if(sigint_set) {
    sigint_set = 0;
    VM_ASSERT(0, "received SIGINT");
  }

  if(pc >= num_codes) {
    VM_ASSERT(0, "pc %ld flew off the end of memory %ld", pc, num_codes);
  }

  opcode = (unsigned char)(long)codes[pc];
  pc += 3;			/* instructions are 3 bytes wide */

  VM_DEBUG("dispatching", instr);

  switch (opcode) {
  case _pushvarargs_:{
      int ii;

      const int nvarargs = (int)n_args - ARG1;
      object *result = g->empty_list;
      push_root(&result);

      for(ii = 0; ii < nvarargs; ++ii) {
	VPOP(top, stack, stack_top);
	result = cons(top, result);
      }

      VPUSH(result, stack, stack_top);
      pop_root(&result);

      VM_DEBUG("after_args environment", env);
    }
    break;
  case _chainframe_:{
      /* generate a fresh frame for the callee */
      top = make_vector(g->empty_list, ARG1);
      env = cons(top, env);
    }
    break;
  case _endframe_: {
      /* throw away the stack portion of this function's frame, except
	 the top N. */
      int ii;
      const int dist = (stack_top - ARG1) - fn_first_arg;
      if(dist > 0) {
	for(ii = 0; ii < ARG1; ++ii) {
	  VARRAY(stack)[fn_first_arg + ii] = VARRAY(stack)[(stack_top - ARG1) + ii];
	}
      }
      stack_top = fn_first_arg + ARG1;
    }
    break;
  case _spush_:{
      /* push the Nth argument onto the stack */
      top = VARRAY(stack)[fn_first_arg + ARG1];
      VPUSH(top, stack, stack_top);
    }
    break;
  case _sset_:{
      /* set stack position N to the value at the top of the
	 stack. leaves the stack unchanged */
      VARRAY(stack)[fn_first_arg + ARG1] = VARRAY(stack)[stack_top - 1];
    }
    break;
  case _swap_:{
      top = VARRAY(stack)[stack_top - 1];
      VARRAY(stack)[stack_top - 1] = VARRAY(stack)[stack_top - 2];
      VARRAY(stack)[stack_top - 2] = top;
    }
    break;
  case _fjump_:{
      VPOP(top, stack, stack_top);
      if(is_falselike(top)) {
	pc = ARG1 * 3;		/* offsets are in instructions */
      }
    }
    break;
  case _tjump_:{
      VPOP(top, stack, stack_top);
      if(!is_falselike(top)) {
	pc = ARG1 * 3;
      }
    }
    break;
  case _jump_:{
      pc = ARG1 * 3;
    }
    break;
  case _fn_:{
      object *fn_arg = VARRAY(const_array)[ARG1];
      object *new_fn = make_compiled_proc(BYTECODE(fn_arg),
					  env);
      VPUSH(new_fn, stack, stack_top);
    }
    break;
  case _callj_:{
      VPOP(top, stack, stack_top);

      /* unwrap meta */
      if(is_meta(top)) {
	top = METAPROC(top);
      }

      long args_for_call = ARG1;

      /* special case for apply */
      if(args_for_call == -1) {
	/* the args are in a list next, expand those */
	object *args;
	VPOP(args, stack, stack_top);

	args_for_call = 0;
	while(!is_the_empty_list(args)) {
	  VPUSH(car(args), stack, stack_top);
	  args = cdr(args);
	  ++args_for_call;
	}
      }

      fn_first_arg = stack_top - args_for_call;

      if(is_compiled_proc(top) || is_compiled_syntax_proc(top)) {
	fn = top;
	pc = 0;
	n_args = args_for_call;
	env = CENV(fn);
	goto vm_fn_begin;
      }
      else if(is_primitive_proc(top)) {
	/* build the list the target expects for the call */
	long ii;
	object *pfn = top;

	top = pfn->data.primitive_proc.fn(stack, args_for_call, stack_top);
	/* unwind the stack since primitives don't clean up after
	   themselves */
	object *temp;
	for(ii = 0; ii < args_for_call; ++ii) {
	  VPOP(temp, stack, stack_top);
	}

	if(is_primitive_exception(top)) {
	  object *temp = top;
	  VM_ERROR_RESTART(cdr(temp));
	}

	VPUSH(top, stack, stack_top);

	RETURN_OPCODE_INSTRUCTIONS;
      }
      else {
	owrite(stderr, top);
	fprintf(stderr, "\n");
	VM_ASSERT(0, "don't know how to invoke");
      }
    }
    break;
  case _lvar_:{
      int env_num = ARG1;
      int idx = ARG2;

      object *next = env;
      while(env_num-- > 0) {
	next = cdr(next);
      }

      object *data = VARRAY(car(next))[idx];
      VPUSH(data, stack, stack_top);
    }
    break;
  case _lset_:{
      int env_num = ARG1;
      int idx = ARG2;

      object *next = env;
      while(env_num-- > 0) {
	next = cdr(next);
      }

      VARRAY(car(next))[idx] = VARRAY(stack)[stack_top - 1];
    }
    break;
  case _gvar_:{
      object *var = VARRAY(const_array)[ARG1];
      object *val;

      /* see if we can get it from cache */
      if(is_pair(var)) {
	val = var;
      }
      else {
	val = lookup_global_value(VARRAY(const_array)[ARG1], g->vm_env);
	if(is_primitive_exception(val)) {
	  VM_ERROR_RESTART(cdr(val));
	}
	VARRAY(const_array)[ARG1] = val;
      }

      VPUSH(cdr(val), stack, stack_top);
    }
    break;
  case _gset_:{
      object *var = VARRAY(const_array)[ARG1];
      object *val = VARRAY(stack)[stack_top - 1];
      object *slot = get_hashtab(g->vm_env, var, NULL);
      if(slot) {
	set_cdr(slot, val);
      }
      else {
	val = cons(var, val);
	define_global_variable(var, val, g->vm_env);
      }
    }
    break;
  case _setcc_:{
      object *new_stack_top;

      VPOP(top, stack, stack_top);
      VPOP(new_stack_top, stack, stack_top);

      /* need to copy the stack into the current stack */
      stack_top = LONG(new_stack_top);

      stack = make_vector(g->empty_list, stack_top);
      long idx;
      for(idx = 0; idx < stack_top; ++idx) {
	VARRAY(stack)[idx] = VARRAY(top)[idx];
      }
    }
    break;
  case _cc_:{
      object *cc_env = make_vector(g->empty_list, 2);
      push_root(&cc_env);

      /* copy the stack */
      object *new_stack = make_vector(g->empty_list, stack_top);
      long idx;
      for(idx = 0; idx < stack_top; ++idx) {
	VARRAY(new_stack)[idx] = VARRAY(stack)[idx];
      }

      /* insert it into the environment */
      VARRAY(cc_env)[0] = new_stack;
      VARRAY(cc_env)[1] = make_fixnum(stack_top);

      cc_env = cons(cc_env, g->empty_list);

      object *cc_fn = make_compiled_proc(g->cc_bytecode, cc_env);
      pop_root(&cc_env);

      VPUSH(cc_fn, stack, stack_top);
    }
    break;
  case _incprof_:{
      if(profiling_enabled) {
	/* disabled temporarily ARG1++; */
      }
    }
    break;
  case _pop_:{
      VPOP(top, stack, stack_top);
    }
    break;
  case _save_:{
      VPUSH(env, stack, stack_top);
      VPUSH(fn, stack, stack_top);
      VPUSH(make_small_fixnum(ARG1 * 3), stack, stack_top);
      VPUSH(make_small_fixnum(fn_first_arg), stack, stack_top);
    }
    break;
  case _return_:{
      RETURN_OPCODE_INSTRUCTIONS;
    }
    break;
  case _const_:{
      long idx = ARG1;
      VPUSH(VARRAY(const_array)[idx], stack, stack_top);
    }
    break;
  default:{
      VM_ASSERT(0, "don't know how to process opcode %d", (int)opcode);
    }
  }

  goto vm_begin;
  VM_RETURN(g->error_sym);
}

DEFUN1(vm_tag_macro_proc) {
  FIRST->type = COMPILED_SYNTAX_PROC;
  return FIRST;
}


DEFUN1(set_cc_bytecode) {
  g->cc_bytecode = BYTECODE(FIRST);
  return FIRST;
}

DEFUN1(set_profiling_proc) {
  if(is_falselike(FIRST)) {
    profiling_enabled = 0;
  }
  else {
    profiling_enabled = 1;
  }
  return FIRST;
}

DEFUN1(set_error_restart_proc) {
  g->vm_error_restart = FIRST;
  return FIRST;
}

#define generate_syminit(opcode) opcode ## _op = make_symbol("" # opcode);

void vm_definer(char *sym, object * value) {
  object *symbol = make_symbol(sym);
  object *slot = get_hashtab(g->vm_env, symbol, NULL);

  if(slot) {
    set_cdr(slot, value);
  }
  else {
    push_root(&value);
    value = cons(symbol, value);
    define_global_variable(symbol, value, g->vm_env);
    pop_root(&value);
  }
}

void vm_boot(void) {
  /* generate the symbol initializations */
  opcode_table(generate_syminit)

    /* register for sigint */
  struct sigaction sa;
  sa.sa_handler = vm_sigint_handler;
  sigaction(SIGINT, &sa, NULL);
}

void vm_add_roots(void) {
  push_root(&(g->cc_bytecode));
  push_root(&(g->vm_error_restart));
}

void vm_init(void) {
  vm_boot();

  vm_definer("set-macro!", make_primitive_proc(vm_tag_macro_proc));

  vm_definer("set-cc-bytecode!", make_primitive_proc(set_cc_bytecode));

  vm_definer("set-error-restart!",
	     make_primitive_proc(set_error_restart_proc));

  g->cc_bytecode = g->empty_list;
  push_root(&(g->cc_bytecode));

  g->vm_error_restart = g->empty_list;
  push_root(&(g->vm_error_restart));

  profiling_enabled = 0;
}

void vm_init_environment(definer defn) {

  defn("symbol->bytecode", make_primitive_proc(symbol_to_code_proc));

  defn("bytecode->symbol", make_primitive_proc(code_to_symbol_proc));

  defn("set-profiling!", make_primitive_proc(set_profiling_proc));

  defn("make-bytecode-array", make_primitive_proc(make_bytecode_array_proc));

  defn("bytecode-ref", make_primitive_proc(get_bytecode_proc));

  defn("bytecode-set!", make_primitive_proc(set_bytecode_proc));
}

void wb(object * fn) {
  long idx = 0;
  object *code_array = car(BYTECODE(fn));
  object *arg_vector = cadr(BYTECODE(fn));

  long size = VSIZE(arg_vector);
  char *codes = ALIEN_PTR(code_array);
  object **args = VARRAY(arg_vector);

  fprintf(stderr, "#<bytecode: ");
  for(idx = 0; idx < size; ++idx) {
    int code = (int)codes[idx];
    fprintf(stderr, "(%s . ", bytecode_str[code]);
    owrite(stderr, args[idx]);
    fprintf(stderr, ") ");
  }
  fprintf(stderr, ">\n");
}
