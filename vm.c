#include <stdio.h>
#include <stdlib.h>

#include "vm.h"
#include "types.h"
#include "symbols.h"
#include "interp.h"
#include "read.h"
#include "gc.h"

#define OPCODE(x) first(x)
#define ARGS(x) cdr(x)
#define ARG1(x) second(x)
#define ARG2(x) third(x)

#define fn_tag(x) first(x)
#define fn_unassembled(x) first(cdr(x))
#define fn_env(x) second(cdr(x))
#define fn_name(x) third(cdr(x))
#define fn_args(x) fourth(cdr(x))
#define fn_bytecode(x) fifth(cdr(x))

#define length1(x) (cdr(x) == the_empty_list)

object *args_op;
object *return_op;
object *fn_op;
object *fjump_op;
object *tjump_op;
object *callj_op;
object *lvar_op;
object *save_op;

object *plus_op;
object *numeq_op;
object *cons_op;

object *error_sym;

#define VM_ASSERT(test, msg)			\
  do {						\
    if(!(test)) {				\
      fprintf(stderr, "%s\n", msg);		\
      VM_RETURN(error_sym);			\
    }						\
  } while(0)

object *plus_impl(object *a, object *b) {
  return make_fixnum(LONG(a) + LONG(b));
}

object *numeq_impl(object *a, object *b) {
  return AS_BOOL(LONG(a) == LONG(b));
}

object *cons_impl(object *a, object *b) {
  return cons(a, b);
}


#define PUSH(obj, stack)			\
  do {						\
    stack = cons(obj, stack);			\
  } while(0)

#define POP(top, stack)				\
  do {						\
    top = car(stack);				\
    stack = cdr(stack);				\
  } while(0)

#define RESET_TOP(pos, obj)			\
  object *p1 = pos;				\
  set_car(p1, obj);				\
  stack = pos

#define CALL1(fn)				\
  do {						\
    object *result = fn(first(stack));		\
    RESET_TOP(stack, result);			\
  } while(0)

#define CALL2(fn)				\
  do {						\
    object *result = fn(second(stack),		\
			first(stack));		\
    RESET_TOP(cdr(stack), result);		\
  } while(0)

#define VM_RETURN(obj)				\
  do {						\
    pop_root(&top);				\
    pop_root(&stack);				\
    return obj;					\
  } while(0)

#define VM_DEBUG(msg, obj)			\
  do {						\
    fprintf(stdout, "%s: ", msg);		\
    write(stdout, obj);				\
    fprintf(stdout, "\n");			\
  } while(0)

object *vm_execute(object *fn) {
  object *code_array;
  object *env;
  object *instr;
  object *opcode;
  long n_args;
  long pc = 0;

  instr = the_empty_list;
  n_args = 0;

  object *stack = the_empty_list;
  push_root(&stack);

  object *top = the_empty_list;
  push_root(&top);

 vm_fn_begin:

  code_array = fn_bytecode(fn);
  env = fn_env(fn);
  pc = 0;
  VM_DEBUG("environment", env);

  object **codes = VARRAY(code_array);
  long num_codes = VSIZE(code_array);

 vm_begin:
  VM_ASSERT(pc < num_codes,
	    "pc flew off the end of memory");

  instr = codes[pc++];
  opcode = OPCODE(instr);

  VM_DEBUG("dispatching", instr);
  /* VM_DEBUG("stack", stack); */

  switch(opcode->type) {
  case BOOLEAN:
  case FIXNUM:
    PUSH(opcode, stack);
    break;

  case SYMBOL:
    if(opcode == args_op) {
      VM_ASSERT(n_args == LONG(ARG1(instr)),
		"wrong number of args");
      
      int ii;
      int num_args = LONG(ARG1(instr));
      object *vector = make_vector(the_empty_list,
				   num_args);
      push_root(&vector);
      PUSH(vector, env);
      pop_root(&vector);

      object **vdata = VARRAY(vector);
      for(ii = num_args - 1; ii >= 0; --ii) {
	POP(top, stack);
	vdata[ii] = top;
      }

      VM_DEBUG("after_args environment", env);
    }
    else if(opcode == plus_op) {
      CALL2(plus_impl);
    }
    else if(opcode == numeq_op) {
      CALL2(numeq_impl);
    }
    else if(opcode == fjump_op) {
      POP(top, stack);
      if(top == false) {
	pc = LONG(ARG1(instr));
      }
    }
    else if(opcode == fn_op) {
      PUSH(ARG1(instr), stack);
    }
    else if(opcode == callj_op) {
      POP(top, stack);
      fn = top;
      n_args = LONG(ARG1(instr));
      goto vm_fn_begin;
    }
    else if(opcode == lvar_op) {
      int env_num = LONG(ARG1(instr));
      int idx = LONG(ARG2(instr));

      object *next = env;
      while(env_num-- > 0) {
	next = cdr(next);
      }

      object *data = VARRAY(car(next))[idx];
      PUSH(data, stack);
    }
    else if(opcode == cons_op) {
      CALL2(cons_impl);
    }
    else if(opcode == save_op) {
      object *ret_addr = cons(fn, env);
      push_root(&ret_addr);
      ret_addr = cons(ARG1(instr), ret_addr);
      PUSH(ret_addr, stack);
      pop_root(&ret_addr);
    }
    else if(opcode == return_op) {
      /* if there's only one value on the stack,
       * we're done */
      if(length1(stack)) {
	VM_RETURN(car(stack));
      } else {
	object *val = first(stack);
	object *ret_addr = second(stack);
	fn = car(cdr(ret_addr));
	pc = LONG(car(ret_addr));
	env = cdr(cdr(ret_addr));

	code_array = fn_bytecode(fn);
	codes = VARRAY(code_array);
	num_codes = VSIZE(code_array);
	stack = cons(val, cdr(cdr(stack)));
      }
    }
    else {
      fprintf(stderr, "don't know how to process ");
      write(stderr, opcode);
      fprintf(stderr, "\n");
      VM_ASSERT(0, "strange opcode");
    }
    break;

  default:
    fprintf(stderr, "don't know how to process ");
    write(stderr, opcode);
    fprintf(stderr, "\n");
    VM_ASSERT(0, "strange opcode");
  }

  goto vm_begin;

  VM_RETURN(error_sym);
}

void vm_init(void) {
  args_op = make_symbol("args");
  return_op = make_symbol("return");
  fn_op = make_symbol("fn");
  fjump_op = make_symbol("fjump");
  tjump_op = make_symbol("tjump");
  callj_op = make_symbol("callj");
  lvar_op = make_symbol("lvar");
  save_op = make_symbol("save");

  plus_op = make_symbol("+");
  numeq_op = make_symbol("=");
  cons_op = make_symbol("cons");

  error_sym = make_symbol("error");
}
