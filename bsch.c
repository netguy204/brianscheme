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
#include <unistd.h>

#include "interp.h"
#include "read.h"
#include "gc.h"
#include "ffi.h"

char **bs_paths;

char **split_path(char *path) {
  /* Count delimiters while making a copy. */
  char del = ':';
  int count = 1;
  char *copy = MALLOC(strlen(path) + 1), *cp = copy, *p = path;
  while (*p != '\0') {
    *cp = *p;
    if (*p == del)
      count++;
    cp++;
    p++;
  }
  *cp = '\0';

  cp = copy;
  char **r = MALLOC((count + 1) * sizeof(char**));
  int i;
  for (i = 0; i < count; i++) {
    r[i] = cp;
    cp = strchr(cp, del);
    if (cp != NULL)
      *cp = '\0';
    cp++;
  }
  r[count] = NULL;
  return r;
}

/* Allocate new string and concat strings in it separated by a /. */
char *pathcat(char *a, char *b) {
  size_t alen = strlen(a);
  size_t blen = strlen(b);
  char *out = MALLOC(alen + blen + 2);
  strcpy(out, a);
  *(out + alen) = '/';
  strcpy(out + alen + 1, b);
  *(out + alen + blen + 1) = '\0';
  return out;
}

object * load_library(char *libname) {
  char *filename;
  char **paths = bs_paths;
  while (*paths != NULL) {
    filename = pathcat(*paths, libname);
    if (access(filename, R_OK) == 0)
      break;
    free(filename);
    filename = NULL;
    paths++;
  }
  if(filename == NULL) {
    fprintf(stderr, "Failed to load %s. Is BS_PATH right?\n", libname);
    exit(3);
  }
  FILE * stdlib = fopen(filename, "r");
  if(stdlib == NULL) {
    fprintf(stderr, "Somehow failed to load %s after it existed.\n", libname);
    exit(3);
  }
  free(filename);

  object *form;
  object *result = NULL;
  while((form = obj_read(stdlib)) != NULL) {
    push_root(&form);
    result = interp(form, the_empty_environment);
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

  /* Handle BS_PATH */
  char *path = getenv("BS_PATH");
  if (path == NULL)
    path = ".";
  char **paths = bs_paths = split_path(path);
  object* list = the_empty_list;
  object* str = the_empty_list;
  push_root(&list);
  push_root(&str);
  while (*paths != NULL) paths++;
  while (paths > bs_paths) {
    /* Build up list in reverse. */
    paths--;
    str = make_string(*paths);
    list = cons(str, list);
  }
  pop_root(&str);
  object *sym = make_symbol("*load-path*");
  define_global_variable(sym, list, the_global_environment);
  define_global_variable(sym, list, vm_global_environment);
  pop_root(&list);

  if(argc > 1 && strcmp(argv[1], "-b") == 0) {
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
