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
#include <getopt.h>

#include "interp.h"
#include "read.h"
#include "gc.h"
#include "ffi.h"
#include "vm.h"

char *version = "Mercury";

/* Options */
char *progname;
char **bs_paths;
int bootstrap = 1;
int print_help = 0;
char *image = NULL;

void print_usage(int ret) {
  printf ("Usage: %s [options] [script [arguments]]\n", progname);
  printf ("\t-b           Do not bootstrap\n");
  printf ("\t-l           Load an image\n");
  printf ("\t-v           Print version information\n");
  printf ("\t-h           Print this usage text\n");
  exit(ret);
}

void print_version ()
{
  printf ("BrianScheme, version %s.\n", version);
  exit(EXIT_SUCCESS);
}

char **split_path(char *path) {
  /* Count delimiters while making a copy. */
  char del = ':';
  int count = 1;
  char *copy = xmalloc(strlen(path) + 1), *cp = copy, *p = path;
  while (*p != '\0') {
    *cp = *p;
    if (*p == del)
      count++;
    cp++;
    p++;
  }
  *cp = '\0';

  cp = copy;
  char **r = xmalloc((count + 1) * sizeof(char**));
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
  char *out = xmalloc(alen + blen + 2);
  strcpy(out, a);
  *(out + alen) = '/';
  strcpy(out + alen + 1, b);
  *(out + alen + blen + 1) = '\0';
  return out;
}

void insert_strlist(char **strv, char *name, int use_interp) {
  char **strs = strv;
  object* list = g->empty_list;
  object* str = g->empty_list;
  push_root(&list);
  push_root(&str);
  while (*strs != NULL) strs++;
  while (strs > strv) {
    /* Build up list in reverse. */
    strs--;
    str = make_string(*strs);
    list = cons(str, list);
  }
  pop_root(&str);

  if (use_interp)
    interp_definer(name, list);
  vm_definer(name, list);
  pop_root(&list);
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
    result = interp(form, g->empty_env);
    pop_root(&form);
  }

  fclose(stdlib);
  return result;
}

object * compile_library(char *libname) {
  object *compile_file = make_symbol("compile-file");
  object *compiler = get_hashtab(g->vm_env, compile_file, NULL);
  if(compiler == NULL) {
    fprintf(stderr, "compile-file is not defined\n");
    exit(4);
  } else {
    compiler = cdr(compiler);
  }

  object *form = g->empty_list;
  push_root(&form);
  form = cons(make_string(libname), form);
  object *result = apply(compiler, form);
  pop_root(&form);
  return result;
}

/* SFX stuff. */
#include <inttypes.h>

char temp_image[L_tmpnam];

off_t find_image() {
  size_t ps = sysconf(_SC_PAGE_SIZE);
  FILE *self = fopen("/proc/self/exe", "r");
  uint32_t sample;
  off_t loc = 0;
  while (1) {
    int r = fseek(self, ps - 4, SEEK_CUR);
    if (r < 0) break;
    r = fread(&sample, 4, 1, self);
    if (r == 0) break;
    loc += ps;
    if (sample == 0xdeadbeef) {
      fclose(self);
      return loc;
    }
  }
  fclose(self);
  return 0;
}
/* End SFX stuff. */

int main(int argc, char ** argv) {
  int ii;
  progname = argv[0];

  /* Handle BS_PATH */
  char *path = getenv("BS_PATH");
  if (path == NULL)
    path = ".";
  bs_paths = split_path(path);

  /* Handle command line arguments. */
  int c;
  while ((c = getopt(argc, argv, "+bhvl:")) != -1)
    switch (c)
      {
      case 'b':
	bootstrap = 0;
	break;
      case 'l':
	image = optarg;
	break;
      case 'v':
	print_version ();
	break;
      case 'h':
	print_help = 1;
	break;
      case '?':
	print_usage (EXIT_FAILURE);
	break;
      }
  if (print_help)
    print_usage (EXIT_SUCCESS);

  off_t img_off = 0;
#ifdef SFX
  if (image == NULL) {
    /* Always load an image. */
    img_off = find_image();
    image = "/proc/self/exe";
  }
#endif

  if (image) {
    int r = load_image(image, img_off);
    if (r != 0) {
      exit(EXIT_FAILURE);
    }

    /* need to reset the roots since some point to our old stack */
    gc_boot();
    interp_add_roots();
    vm_add_roots();
    ffi_add_roots();

    /* the vm needs to build some tables */
    vm_boot();

    /* need to patch up some things that move between boots */
    patch_object(g->stdin_symbol, make_input_port(stdin));
    patch_object(g->stdout_symbol, make_output_port(stdout));
    patch_object(g->stderr_symbol, make_output_port(stderr));

    /* Stick arguments and BS_PATH in global environment. */
    insert_strlist(bs_paths, "*load-path*", 0);
    insert_strlist(argv + optind, "*args*", 0);

    /* Fire up a REPL, or whatever the user had in mind. */
    apply(cdr(get_hashtab(g->vm_env, make_symbol("*image-start*"), NULL)),
	  g->empty_list);
    exit(0);
  }

  init();

  /* Stick arguments and BS_PATH in global environment. */
  insert_strlist(bs_paths, "*load-path*", 1);
  insert_strlist(argv + optind, "*args*", 1);

  if(!bootstrap) {
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
