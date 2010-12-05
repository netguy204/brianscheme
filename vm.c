#include <stdio.h>
#include <stdlib.h>

#include "symbols.h"
#include "interp.h"
#include "read.h"
#include "gc.h"

#define OPCODE(x) first(x)
#define ARGS(x) cdr(x)
#define ARG1(x) second(x)

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
object *plus_op;
object *numeq_op;

void vm_assert(char test, char *msg) {
  if(!test) {
    fprintf(stderr, "%s\n", msg);
    exit(1);
  }
}

object *plus_impl(object *a, object *b) {
  return make_fixnum(LONG(a) + LONG(b));
}

object *numeq_impl(object *a, object *b) {
  return AS_BOOL(LONG(a) == LONG(b));
}

#define PUSH_STACK(obj)				\
  do {						\
    stack = cons(obj, stack);			\
  } while(0)

#define POP_STACK(top)				\
  do {						\
    top = car(stack);				\
    stack = cdr(stack);				\
  } while(0)

#define RESET_TOP(pos, obj)			\
  object *p1 = pos;				\
  set_car(p1, obj);				\
  set_cdr(p1, the_empty_list);			\
  stack = pos

#define CALL1(fn)				\
  do {						\
    object *result = fn(first(stack));		\
    RESET_TOP(cdr(stack), result);		\
  } while(0)

#define CALL2(fn)				\
  do {						\
    object *result = fn(second(stack),		\
			first(stack));		\
    RESET_TOP(cddr(stack), result);		\
  } while(0)

#define VM_RETURN(obj)				\
  do {						\
    pop_root(&top);				\
    pop_root(&stack);				\
    return obj;					\
  } while(0)

object *vm_execute(object *fn) {
  object *code_array = fn_bytecode(fn);
  object *env = fn_env(fn);
  object *instr = the_empty_list;
  object *opcode;
  long n_args = 0;
  long pc = 0;

  object *stack = the_empty_list;
  push_root(&stack);

  object *top = the_empty_list;
  push_root(&top);

  object **codes = VARRAY(code_array);
  long num_codes = VSIZE(code_array);

 vm_begin:
  vm_assert(pc < num_codes,
	    "pc flew off the end of memory");

  instr = codes[pc++];
  opcode = OPCODE(instr);

  switch(opcode->type) {
  case BOOLEAN:
  case FIXNUM:
    PUSH_STACK(opcode);
    break;

  case SYMBOL:
    if(opcode == args_op) {
      vm_assert(n_args == LONG(ARG1(instr)),
		"wrong number of args");
    }
    else if(opcode == return_op) {
      /* if there's only one value on the stack,
       * we're done */
      if(length1(stack)) {
	VM_RETURN(car(stack));
      } else {
	vm_assert(0, "can't handle a vm return yet");
	VM_RETURN(NULL);
      }
    }
    else if(opcode == plus_op) {
      CALL2(plus_impl);
    }
    else if(opcode == numeq_op) {
      CALL2(numeq_impl);
    }
    else if(opcode == fjump_op) {
      POP_STACK(top);
      if(top == false) {
	pc = LONG(ARG1(instr));
      }
    }
    else {
      fprintf(stderr, "don't know how to process ");
      write(stderr, opcode);
      fprintf(stderr, "\n");
      vm_assert(0, "strange opcode");
    }
    break;

  default:
    fprintf(stderr, "don't know how to process ");
    write(stderr, opcode);
    fprintf(stderr, "\n");
    vm_assert(0, "strange opcode");
  }

  goto vm_begin;
  vm_assert(0, "don't know how I got here");
  VM_RETURN(NULL);
}

void vm_init(void) {
  args_op = make_symbol("args");
  return_op = make_symbol("return");
  fn_op = make_symbol("fn");
  fjump_op = make_symbol("fjump");
  tjump_op = make_symbol("tjump");
  plus_op = make_symbol("+");
  numeq_op = make_symbol("=");
}

int main(int argc, char** argv) {
  init();
  vm_init();

  if(argc != 2) {
    fprintf(stderr, "usage: %s bytecode\n", argv[0]);
    exit(1);
  }

  char *bfilename = argv[1];
  FILE *bfile = fopen(bfilename, "r");

  if(bfile == NULL) {
    fprintf(stderr, "failed to open %s\n", bfilename);
    exit(1);
  }

  object *bc = read(bfile);
  fclose(bfile);

  if(bc == NULL) {
    fprintf(stderr, "failed to read bytecode from %s\n",
	    bfilename);
    exit(1);
  }

  write(stdout, bc);
  printf("\n");

  object *result = vm_execute(bc);
  printf("result: ");
  write(stdout, result);
  printf("\n");

  return 0;
}
