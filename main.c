#include <stdlib.h>
#include <string.h>

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

  object *form;
  while((form = obj_read(stdlib)) != NULL) {
    push_root(&form);
    print_obj(interp(form, the_global_environment));
    pop_root(&form);
  }

  fclose(stdlib);
}

int main(int argc, char ** argv) {
  int ii;
  char *libname = "stdlib.sch";

  init();

  if(argc > 1 && strcmp(argv[1], "-b") == 0) {
    /* don't load the standard lib */
    ii = 2;
  } else {
    /* load the stdlib */
    load_library(libname);
    ii = 1;
  }

  for(; ii < argc; ++ii) {
    load_library(argv[ii]);
  }

  object *input;
  while((input = obj_read(stdin)) != NULL) {
    push_root(&input);
    print_obj(interp(input, the_global_environment));
    pop_root(&input);
  }
  return 0;
}
