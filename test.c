#include "interp.h"
#include "types.h"

#include <stdio.h>
#include <assert.h>

void print_obj(object *obj) {
  write(stdout, obj);
  printf("\n");
}

int main(int argc, char ** argv) {
  object *env;

  init();

  /* try looking up a few things in the environment */
  printf("in base-env\n");
  env = the_global_environment;
  lookup_variable_value(make_symbol("+"), env);
  lookup_variable_value(make_symbol("-"), env);
  lookup_variable_value(make_symbol("*"), env);
  lookup_variable_value(make_symbol("list"), env);

  /* now extend the environment and try again */
  env = extend_environment(the_empty_list,
			   the_empty_list,
			   env);

  printf("in sub-env\n");
  lookup_variable_value(make_symbol("+"), env);
  lookup_variable_value(make_symbol("-"), env);
  lookup_variable_value(make_symbol("*"), env);
  lookup_variable_value(make_symbol("list"), env);

  printf("interp\n");
  object *xsym = make_symbol("x");
  object *one = make_fixnum(1);

  /* (lambda (x) (+ 3 x)) */
  object *ldef =
    cons(lambda_symbol,
	 cons(cons(xsym, the_empty_list),
	      cons(cons(make_symbol("+"),
			 cons(make_fixnum(3),
			      cons(xsym, the_empty_list))),
		   the_empty_list)));
  print_obj(ldef);

  /* define the function */
  object *fn = interp(ldef, env);
  assert(is_compound_proc(fn));
  print_obj(fn);
  
  /* apply the function */
  object *fnapply =
    cons(fn, cons(one, the_empty_list));
  object *fnres = interp(fnapply, env);
  assert(is_fixnum(fnres));
  print_obj(fnres);

  object *two = make_fixnum(2);
  fnapply = 
    cons(fn, cons(two, the_empty_list));
  fnres = interp(fnapply, env);
  assert(is_fixnum(fnres));
  print_obj(fnres);

  /* scheme also evaluates head before application */
  fnapply = 
    cons(ldef, cons(two, the_empty_list));
  fnres = interp(fnapply, env);
  assert(is_fixnum(fnres));
  print_obj(fnres);

  /* try defining a symbol */
  object *defsym = cons(make_symbol("set!"),
			cons(make_symbol("foo"),
			     cons(ldef, the_empty_list)));
  print_obj(defsym);
  print_obj(interp(defsym, env));

  /* now apply it */
  object *symcall = cons(make_symbol("foo"),
			 cons(make_fixnum(10), the_empty_list));
  print_obj(symcall);
  print_obj(interp(symcall, env));

  return 0;
}
