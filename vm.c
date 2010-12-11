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

#define length1(x) (cdr(x) == the_empty_list)

object *args_op;
object *return_op;
object *fn_op;
object *fjump_op;
object *tjump_op;
object *callj_op;
object *lvar_op;
object *save_op;
object *gvar_op;
object *lset_op;
object *pop_op;

object *plus_op;
object *numeq_op;
object *cons_op;
object *null_op;
object *car_op;
object *cdr_op;

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
    pop_root(&env);				\
    pop_root(&stack);				\
    return obj;					\
  } while(0)

/** this code needs to be spliced into several
 *  locations so we just define it once here
 */
#define RETURN_OPCODE_INSTRUCTIONS		\
  /* if there's only one value on the stack,	\
   * we're done */                              \
  if(length1(stack)) {				\
    VM_RETURN(car(stack));			\
  } else {					\
    object *val = first(stack);			\
    object *ret_addr = second(stack);		\
    /* retore what we stashed away in save */	\
    fn = car(cdr(ret_addr));			\
    pc = LONG(car(ret_addr));			\
    env = cdr(cdr(ret_addr));			\
    /* setup for the next loop */		\
    stack = cons(val, cdr(cdr(stack)));		\
    goto vm_fn_begin;				\
  }

#ifdef VM_DEBUGGING
#define VM_DEBUG(msg, obj)			\
  do {						\
    fprintf(stdout, "%s: ", msg);		\
    write(stdout, obj);				\
    fprintf(stdout, "\n");			\
  } while(0)
#else
#define VM_DEBUG(msg, obj)
#endif

object *vm_execute(object *fn, object *stack,
		   object *ienv) {
  object *code_array;
  object *env;
  object *instr;
  object *opcode;
  object *top;

  long n_args = 0;
  long pc = 0;

  /* count the args */
  object *next = stack;
  while(!is_the_empty_list(next)) {
    ++n_args;
    next = cdr(next);
  }

  env = CENV(fn);
  instr = the_empty_list;
  top = the_empty_list;

  push_root(&stack);
  push_root(&env);
  push_root(&top);

  VM_ASSERT(is_compiled_proc(fn),
	    "object is not compiled-procedure");

 vm_fn_begin:
  code_array = BYTECODE(fn);
  VM_DEBUG("bytecode", code_array);
  VM_DEBUG("stack", stack);

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
      object *fn_arg = ARG1(instr);
      object *new_fn = make_compiled_proc(BYTECODE(fn_arg),
					  env);
      push_root(&new_fn);
      PUSH(new_fn, stack);
      pop_root(&new_fn);
    }
    else if(opcode == callj_op) {
      POP(top, stack);
      if(is_compiled_proc(top)) {
	fn = top;
	env = CENV(fn);
	pc = 0;
	n_args = LONG(ARG1(instr));

	goto vm_fn_begin;

      } else if(is_primitive_proc(top)
		|| is_compound_proc(top)) {

	/* push the stack arguments onto a new list
	   to hand off to the primitive */
	int args_for_call = LONG(ARG1(instr));
	int ii;

	object *pfn = top;
	push_root(&fn);
	object *arglist = the_empty_list;
	push_root(&arglist);

	for(ii = 0; ii < args_for_call; ++ii) {
	  POP(top, stack);
	  arglist = cons(top, arglist);
	}

	if(is_primitive_proc(pfn)) {
	    top = pfn->data.primitive_proc.fn(arglist, ienv);
	} else {
	  object *call_env = extend_environment(pfn->data.compound_proc.parameters,
						arglist,
						pfn->data.compound_proc.env);
	  push_root(&call_env);
	  top = interp(pfn->data.compound_proc.body, call_env);
	  pop_root(&call_env);
	}

	PUSH(top, stack);

	pop_root(&arglist);
	pop_root(&fn);

	/* generate return */
	RETURN_OPCODE_INSTRUCTIONS;
      } else if(is_compound_proc(top)) {
	VM_ASSERT(0, "compound invocation not implemented");
	/* generate return */
      } else {
	VM_ASSERT(0, "unrecognized invocation");
      }
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
    else if(opcode == lset_op) {
      int env_num = LONG(ARG1(instr));
      int idx = LONG(ARG2(instr));

      object *next = env;
      while(env_num-- > 0) {
	next = cdr(next);
      }

      VARRAY(car(next))[idx] = first(stack);
    }
    else if(opcode == gvar_op) {
      object *var = lookup_variable_value(ARG1(instr), ienv);
      push_root(&var);
      PUSH(var, stack);
      pop_root(&var);
    }
    else if(opcode == pop_op) {
      POP(top, stack);
    }
    else if(opcode == cons_op) {
      CALL2(cons_impl);
    }
    else if(opcode == null_op) {
      POP(top, stack);
      if(is_the_empty_list(top)) {
	PUSH(true, stack);
      } else {
	PUSH(false, stack);
      }
    }
    else if(opcode == car_op) {
      set_car(stack,
	      car(car(stack)));
    }
    else if(opcode == cdr_op) {
      set_car(stack,
	      cdr(car(stack)));
    }
    else if(opcode == save_op) {
      object *ret_addr = cons(fn, env);
      push_root(&ret_addr);
      ret_addr = cons(ARG1(instr), ret_addr);
      PUSH(ret_addr, stack);
      pop_root(&ret_addr);
    }
    else if(opcode == return_op) {
      RETURN_OPCODE_INSTRUCTIONS;
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
  gvar_op = make_symbol("gvar");
  lset_op = make_symbol("lset");
  pop_op = make_symbol("pop");

  plus_op = make_symbol("+");
  numeq_op = make_symbol("=");
  cons_op = make_symbol("cons");
  null_op = make_symbol("null?");
  car_op = make_symbol("car");
  cdr_op = make_symbol("cdr");

  error_sym = make_symbol("error");
}
