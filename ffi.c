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

#include <string.h>
#include <stdlib.h>
#include <dlfcn.h>

#include <ffi.h>
#include "types.h"
#include "interp.h"
#include "gc.h"

/* useful offsets for manipulating objects from userspace */
unsigned int fixnum_offset;
unsigned int car_offset;
unsigned int cdr_offset;

object *free_ptr_fn;
object *ffi_release_type_fn;

object *ffi_type_pointer_sym;
object *ffi_type_void_sym;
object *ffi_type_uchar_sym;
object *ffi_type_ushort_sym;
object *ffi_type_uint_sym;
object *ffi_type_sint_sym;
object *ffi_type_ulong_sym;

typedef void (*FN_PTR) (void);

DEFUN1(dlopen_proc) {
  void *handle;

  if(FIRST == the_empty_list) {
    handle = dlopen(NULL, RTLD_LAZY);
  }
  else {
    handle = dlopen(STRING(FIRST), RTLD_LAZY | RTLD_GLOBAL);
  }

  if(handle == NULL) {
    return false;
  }
  else {
    return make_alien(handle, the_empty_list);
  }
}

DEFUN1(dlsym_proc) {
  void *handle = ALIEN_PTR(FIRST);

  dlerror();

  FN_PTR fn;
  *(void **)(&fn) = dlsym(handle, STRING(SECOND));

  char *msg;
  if((msg = dlerror()) != NULL) {
    fprintf(stderr, "dlerror: %s\n", msg);
    return false;
  }

  return make_alien_fn(fn, the_empty_list);
}

DEFUN1(dlsym2_proc) {
  void *handle = ALIEN_PTR(FIRST);

  dlerror();
  void *ptr = dlsym(handle, STRING(SECOND));
  char *msg;
  if((msg = dlerror()) != NULL) {
    fprintf(stderr, "dlerror: %s\n", msg);
    return false;
  }

  return make_alien(ptr, the_empty_list);
}



DEFUN1(dlclose_proc) {
  void *handle = ALIEN_PTR(FIRST);
  dlclose(handle);
  return true;
}

DEFUN1(free_ptr) {
  void *ptr = ALIEN_PTR(FIRST);
  free(ptr);
  return true;
}

DEFUN1(free_ffi_alien_object) {
  object *alien = FIRST;
  object *releaser = ALIEN_RELEASER(alien);

  if(is_the_empty_list(releaser)) {
    return false;
  }

  /* build the list to eval */
  object *list = the_empty_list;
  push_root(&list);
  list = cons(alien, list);
  list = cons(releaser, list);

  /* send it to interp and return the result */
  object *result = interp(list, environment);
  pop_root(&list);
  return result;
}

DEFUN1(ffi_make_cif) {
  ffi_cif *cif = MALLOC(sizeof(ffi_cif));
  return make_alien(cif, free_ptr_fn);
}

DEFUN1(ffi_primitive_type) {
  object *type = FIRST;
  ffi_type *tgt_type;
  if(type == ffi_type_pointer_sym) {
    tgt_type = &ffi_type_pointer;
  }
  else if(type == ffi_type_void_sym) {
    tgt_type = &ffi_type_void;
  }
  else if(type == ffi_type_uchar_sym) {
    tgt_type = &ffi_type_uchar;
  }
  else if(type == ffi_type_ushort_sym) {
    tgt_type = &ffi_type_ushort;
  }
  else if(type == ffi_type_uint_sym) {
    tgt_type = &ffi_type_uint;
  }
  else if(type == ffi_type_sint_sym) {
    tgt_type = &ffi_type_sint;
  }
  else if(type == ffi_type_ulong_sym) {
    tgt_type = &ffi_type_ulong;
  }
  else {
    /* unknown type */
    return false;
  }

  return make_alien(tgt_type, the_empty_list);
}

DEFUN1(ffi_make_pointer_array) {
  void **array = MALLOC(sizeof(void *) * LONG(FIRST));
  return make_alien(array, free_ptr_fn);
}

DEFUN1(ffi_set_pointer) {
  void **array = ALIEN_PTR(FIRST);
  long idx = LONG(SECOND);
  void *value = ALIEN_PTR(THIRD);
  array[idx] = value;
  return FIRST;
}

DEFUN1(ffi_get_pointer) {
  void **array = ALIEN_PTR(FIRST);
  long idx = LONG(SECOND);
  return make_alien(array[idx], the_empty_list);
}

DEFUN1(ffi_deref) {
  void **value = ALIEN_PTR(FIRST);
  return make_alien(*value, the_empty_list);
}

DEFUN1(ffi_prep_cif_proc) {
  ffi_cif *cif = ALIEN_PTR(FIRST);
  long n_args = LONG(SECOND);
  ffi_type *rtype = ALIEN_PTR(THIRD);
  ffi_type **args = ALIEN_PTR(FOURTH);

  if(ffi_prep_cif(cif, FFI_DEFAULT_ABI, n_args, rtype, args) == FFI_OK) {
    return true;
  }
  else {
    return false;
  }
}

DEFUN1(ffi_call_proc) {
  ffi_cif *cif = ALIEN_PTR(FIRST);
  FN_PTR fn = ALIEN_FN_PTR(SECOND);
  void *result = ALIEN_PTR(THIRD);
  void **values = ALIEN_PTR(FOURTH);

  ffi_call(cif, fn, result, values);
  return true;
}

void interp_trampoline(ffi_cif *cif, void *ret,
		       void ** args, void * target_ptr) {
  object * target = (object*)target_ptr;
  object * exp = the_empty_list;
  object * alien = make_alien(args, the_empty_list);

  push_root(&exp);
  push_root(&alien);

  exp = cons(alien, exp);
  exp = cons(target, exp);
  interp(exp, the_global_environment);

  pop_root(&alien);
  pop_root(&exp);
}

DEFUN1(create_closure_proc) {
  ffi_cif *cif = ALIEN_PTR(FIRST);
  object *target = SECOND;
  void * ret = THIRD;

  ffi_closure *closure;
  FN_PTR fn_with_closure;
  closure = ffi_closure_alloc(sizeof(ffi_closure), (void**)&fn_with_closure);
  if(!closure) {
    return false;
  }

  if(ffi_prep_closure_loc(closure, cif, interp_trampoline, target, fn_with_closure) != FFI_OK) {
    ffi_closure_free(closure);
    return false;
  }

  return make_alien_fn(fn_with_closure, the_empty_list);
}

/* provides an example function that can be called from userland to
   demonstrate the ffi closure functionality
*/
void test_fn(void (*rfn)(int)) {
  rfn(42);
}

DEFUN1(ffi_address_of) {
  void **ptr = &(ALIEN_PTR(FIRST));
  return make_alien(ptr, the_empty_list);
}

char *strdup(const char *string);

DEFUN1(string_to_alien) {
  char *str = STRING(FIRST);
  return make_alien(strdup(str), free_ptr_fn);
}

DEFUN1(alien_to_string) {
  char *str = ALIEN_PTR(FIRST);
  return make_string(str);
}

DEFUN1(stream_to_alien) {
  FILE *stream = INPUT(FIRST);
  return make_alien(stream, the_empty_list);
}

DEFUN1(int_to_alien) {
  return make_alien((void *)LONG(FIRST), the_empty_list);
}

DEFUN1(alien_to_int) {
  long val = (long)ALIEN_PTR(FIRST);
  return make_fixnum(val);
}

DEFUN1(alien_to_primitive) {
  prim_proc *fn = (prim_proc *) ALIEN_FN_PTR(FIRST);
  return make_primitive_proc(fn);
}


void init_ffi(object * env) {
#define add_procedure(scheme_name, c_name)    \
  define_variable(make_symbol(scheme_name),   \
                  curr=make_primitive_proc(c_name),	\
                  env);

  object *curr = the_empty_list;

  free_ptr_fn = make_primitive_proc(free_ptr);
  push_root(&free_ptr_fn);

  push_root(&curr);
  add_procedure("ffi:dlopen", dlopen_proc);
  add_procedure("ffi:dlsym", dlsym_proc);
  add_procedure("ffi:dlsym-var", dlsym2_proc);
  add_procedure("ffi:dlclose", dlclose_proc);

  add_procedure("ffi:make-cif", ffi_make_cif);
  add_procedure("ffi:make-pointer-array", ffi_make_pointer_array);
  add_procedure("ffi:set-array-pointer!", ffi_set_pointer);
  add_procedure("ffi:get-array-pointer", ffi_get_pointer);
  add_procedure("ffi:primitive", ffi_primitive_type);
  add_procedure("ffi:prep-cif", ffi_prep_cif_proc);
  add_procedure("ffi:call", ffi_call_proc);
  add_procedure("ffi:free", free_ffi_alien_object);

  add_procedure("ffi:string-to-alien", string_to_alien);
  add_procedure("ffi:alien-to-string", alien_to_string);
  add_procedure("ffi:int-to-alien", int_to_alien);
  add_procedure("ffi:alien-to-int", alien_to_int);
  add_procedure("ffi:stream-to-alien", stream_to_alien);
  add_procedure("ffi:alien-to-primitive-proc", alien_to_primitive);
  add_procedure("ffi:create-closure", create_closure_proc);
  add_procedure("ffi:address-of", ffi_address_of);
  add_procedure("ffi:deref", ffi_deref);

  pop_root(&curr);

  ffi_type_pointer_sym = make_symbol("ffi-pointer");
  ffi_type_void_sym = make_symbol("ffi-void");
  ffi_type_uchar_sym = make_symbol("ffi-uchar");
  ffi_type_ushort_sym = make_symbol("ffi-ushort");
  ffi_type_uint_sym = make_symbol("ffi-uint");
  ffi_type_sint_sym = make_symbol("ffi-sint");
  ffi_type_ulong_sym = make_symbol("ffi-ulong");

  /* setup offset values */
  fixnum_offset = (unsigned int)&(((object *) 0)->data.fixnum.value);
  car_offset = (unsigned int)&(((object *) 0)->data.pair.car);
  cdr_offset = (unsigned int)&(((object *) 0)->data.pair.cdr);
}
