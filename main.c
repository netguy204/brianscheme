#include <stdlib.h>
#include "interp.h"
#include "read.h"
#include "gc.h"

void load_library(char *libname) {
  FILE * stdlib = fopen(libname, "r");
  if(stdlib == NULL) {
    fprintf(stderr, "failed to load %s. Is CWD right?\n",
	    libname);
    exit(3);
  }
  
  object *form = read(stdlib);
  push_root(&form);
  while(form) {
    print_obj(interp(form, the_global_environment));
    form = read(stdlib);
  }
  pop_root(&form);
}

int main(int argc, char ** argv) {
  int ii;
  char *libname = "stdlib.sch";

  init();

  /* load the stdlib */
  load_library(libname);

  for(ii = 1; ii < argc; ++ii) {
    load_library(argv[ii]);
  }

  object *input;
  while((input = read(stdin)) != NULL) {
    push_root(&input);
    print_obj(interp(input, the_global_environment));
    pop_root(&input);
  }
  return 0;
}
