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
object *argsdot_op;
object *return_op;
object *const_op;
object *fn_op;
object *fjump_op;
object *tjump_op;
object *jump_op;
object *callj_op;
object *lvar_op;
object *save_op;
object *gvar_op;
object *lset_op;
object *gset_op;
object *pop_op;

object *error_sym;

#define VM_ASSERT(test, msg)			\
  do {						\
    if(!(test)) {				\
      fprintf(stderr, "%s\n", msg);		\
      VM_RETURN(error_sym);			\
    }						\
  } while(0)


#define VPUSH(obj, stack, top)				\
  do {							\
    if(top == VSIZE(stack)) {				\
      long old_size = VSIZE(stack);			\
      VSIZE(stack) = old_size * 1.8;			\
      VARRAY(stack) = realloc(VARRAY(stack),		\
			      sizeof(object*)		\
			      * VSIZE(stack));		\
      int ii;						\
      for(ii = old_size; ii < VSIZE(stack); ++ii) {	\
	VARRAY(stack)[ii] = the_empty_list;		\
      }							\
    }							\
    VARRAY(stack)[top++] = obj;				\
  } while(0)

#define VPOP(tgt, stack, top)			\
  do {						\
    tgt = VARRAY(stack)[--top];			\
    VARRAY(stack)[top] = the_empty_list;	\
  } while(0)


#define VM_RETURN(obj)				\
  do {						\
    pop_root(&top);				\
    pop_root(&env);				\
    pop_root(&fn);				\
    return obj;					\
  } while(0)

/** this code needs to be spliced into several
 *  locations so we just define it once here
 */
#define RETURN_OPCODE_INSTRUCTIONS			\
  /* if there's only one value on the stack,		\
   * we're done */                                      \
  if(stack_top == 1) {					\
    VM_RETURN(VARRAY(stack)[0]);			\
  } else {						\
    object *val;					\
    VPOP(val, stack, stack_top);			\
    object *ret_addr;					\
    VPOP(ret_addr, stack, stack_top);			\
    /* retore what we stashed away in save */		\
    fn = car(cdr(ret_addr));				\
    pc = LONG(car(ret_addr));				\
    env = cdr(cdr(ret_addr));				\
    /* setup for the next loop */			\
    VPUSH(val, stack, stack_top);			\
    goto vm_fn_begin;					\
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
		   long n_args,
		   object *ienv) {
  object *code_array;
  object *env;
  object *instr;
  object *opcode;
  object *top;

  long pc = 0;
  long stack_top = n_args;

  env = CENV(fn);
  instr = the_empty_list;
  top = the_empty_list;

  push_root(&fn);
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

  switch(opcode->type) {
  case BOOLEAN:
  case FIXNUM:
    VPUSH(opcode, stack, stack_top);
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
      env = cons(vector, env);
      pop_root(&vector);

      object **vdata = VARRAY(vector);
      for(ii = num_args - 1; ii >= 0; --ii) {
	VPOP(top, stack, stack_top);
	vdata[ii] = top;
      }

      VM_DEBUG("after_args environment", env);
    }
    else if(opcode == argsdot_op) {
      VM_ASSERT(n_args >= LONG(ARG1(instr)),
		"wrong number of args");
      
      int ii;
      long req_args = LONG(ARG1(instr));
      long array_size = req_args + 1;
      object *vector = make_vector(the_empty_list,
				   array_size);
      push_root(&vector);
      env = cons(vector, env);
      pop_root(&vector);

      object **vdata = VARRAY(vector);
      /* push the excess args onto the last position */
      for(ii = 0; ii < n_args - req_args; ++ii) {
	VPOP(top, stack, stack_top);
	vdata[array_size - 1] = cons(top, vdata[array_size-1]);
      }

      /* now pop off the required args into their positions */
      for(ii = req_args - 1; ii >= 0; --ii) {
	VPOP(top, stack, stack_top);
	vdata[ii] = top;
      }

      VM_DEBUG("after_args environment", env);
    }
    else if(opcode == fjump_op) {
      VPOP(top, stack, stack_top);
      if(is_falselike(top)) {
	pc = LONG(ARG1(instr));
      }
    }
    else if(opcode == tjump_op) {
      if(!is_falselike(top)) {
	pc = LONG(ARG1(instr));
      }
    }
    else if(opcode == jump_op) {
      pc = LONG(ARG1(instr));
    }
    else if(opcode == fn_op) {
      object *fn_arg = ARG1(instr);
      object *new_fn = make_compiled_proc(BYTECODE(fn_arg),
					  env);
      push_root(&new_fn);
      VPUSH(new_fn, stack, stack_top);
      pop_root(&new_fn);
    }
    else if(opcode == callj_op) {
      VPOP(top, stack, stack_top);
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
	  VPOP(top, stack, stack_top);
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

	VPUSH(top, stack, stack_top);

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
      VPUSH(data, stack, stack_top);
    }
    else if(opcode == lset_op) {
      int env_num = LONG(ARG1(instr));
      int idx = LONG(ARG2(instr));

      object *next = env;
      while(env_num-- > 0) {
	next = cdr(next);
      }

      VARRAY(car(next))[idx] = VARRAY(stack)[stack_top - 1];
    }
    else if(opcode == gvar_op) {
      object *var = lookup_variable_value(ARG1(instr), ienv);
      push_root(&var);
      VPUSH(var, stack, stack_top);
      pop_root(&var);
    }
    else if(opcode == gset_op) {
      object *var = ARG1(instr);
      object *val = VARRAY(stack)[stack_top - 1];
      define_variable(var, val, ienv);
    }
    else if(opcode == pop_op) {
      VPOP(top, stack, stack_top);
    }
    else if(opcode == save_op) {
      object *ret_addr = cons(fn, env);
      push_root(&ret_addr);
      ret_addr = cons(ARG1(instr), ret_addr);
      VPUSH(ret_addr, stack, stack_top);
      pop_root(&ret_addr);
    }
    else if(opcode == return_op) {
      RETURN_OPCODE_INSTRUCTIONS;
    }
    else if(opcode == const_op) {
      VPUSH(ARG1(instr), stack, stack_top);
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
  argsdot_op = make_symbol("args.");
  return_op = make_symbol("return");
  const_op = make_symbol("const");
  fn_op = make_symbol("fn");
  fjump_op = make_symbol("fjump");
  tjump_op = make_symbol("tjump");
  jump_op = make_symbol("jump");
  callj_op = make_symbol("callj");
  lvar_op = make_symbol("lvar");
  save_op = make_symbol("save");
  gvar_op = make_symbol("gvar");
  lset_op = make_symbol("lset");
  gset_op = make_symbol("gset");
  pop_op = make_symbol("pop");

  error_sym = make_symbol("error");
}
