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

#include <stdlib.h>
#include <string.h>

#include "interp.h"
#include "read.h"
#include "gc.h"
#include "ffi.h"

object * load_library(char *libname) {
  FILE * stdlib = fopen(libname, "r");
  if(stdlib == NULL) {
    fprintf(stderr, "failed to load %s. Is CWD right?\n",
	    libname);
    exit(3);
  }

  object *form;
  object *result = NULL;
  while((form = obj_read(stdlib)) != NULL) {
    push_root(&form);
    result = interp(form, the_empty_environment);
    print_obj(result);
    pop_root(&form);
  }

  fclose(stdlib);
  return result;
}

object * compile_library(char *libname) {
  object *compile_file = make_symbol("compile-file");
  object *compiler = get_hashtab(vm_global_environment, compile_file, NULL);
  if(compiler == NULL) {
    fprintf(stderr, "compile-file is not defined\n");
    exit(4);
  }

  object *form = the_empty_list;
  push_root(&form);
  form = cons(make_string(libname), form);
  object *result = apply(compiler, form);
  pop_root(&form);
  return result;
}

int main(int argc, char ** argv) {
  int ii;

  init();

  if(argc > 0 && strcmp(argv[1], "-b") == 0) {
    /* don't bootstrap, take the user straight to a totally primitive
       environment */
    for(ii = 2; ii < argc; ++ii) {
      load_library(argv[ii]);
    }
    primitive_repl();
    exit(0);
  }

  /* fist we want to bootstrap the compiled environment */
  object * result = load_library("boot.sch");

  /* if everything went well we should get back a special symbol */
  if(result != make_symbol("finished-compile")) {
    fprintf(stderr, "bootstrap failed. dropping into primitive repl\n");
    primitive_repl();
    exit(1);
  }

  /* now we tear down the interpreter so we can reclaim that memory */
  destroy_interp();

  /* now we load standard lib (which will provide the normal repl for
     the user */
  compile_library("stdlib.sch");

  /* we assume that stdlib will build up an environment that it will
     terminate via non-local exit, so making it here would be an
     error */
  fprintf(stderr, "stdlib.sch returned. this is unexpected.\n");
  return 1;
}
