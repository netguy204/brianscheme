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

void load_library(char *libname) {
  char *filename;
  char **paths = bs_paths;
  while (*paths != NULL) {
    filename = pathcat(*paths, libname);
    printf("checking %s\n", filename);
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

  object *form;
  while((form = obj_read(stdlib)) != NULL) {
    push_root(&form);
    print_obj(interp(form, the_empty_environment));
    pop_root(&form);
  }

  fclose(stdlib);
}

int main(int argc, char ** argv) {
  int ii;
  char *libname = "stdlib.sch";

  init();

  /* Handle BS_PATH */
  char *path = getenv("BS_PATH");
  if (path == NULL)
    path = ".";
  char **paths = bs_paths = split_path(path);
  object* list = the_empty_list;
  while (*paths != NULL) paths++;
  while (paths > bs_paths) {
    /* Build up list in reverse. */
    paths--;
    list = cons(make_string(*paths), list);
  }
  define_global_variable(make_symbol("*load-path*"), list,
			 the_global_environment);

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

  primitive_repl();

  return 0;
}
