#include <string.h>
#include <stdlib.h>

#include <ffi.h>
#include "types.h"
#include "interp.h"
#include "gc.h"

object * free_ptr_fn;
object * ffi_release_type_fn;

object * ffi_type_pointer_sym;
object * ffi_type_uint_sym;

DEFUN1(free_ptr) {
  void * ptr = ALIEN_PTR(FIRST);
  free(ptr);
  return true;
}

DEFUN1(free_ffi_alien_object) {
  object * alien = FIRST;
  object * releaser = ALIEN_RELEASER(alien);

  if(is_the_empty_list(releaser)) {
    return false;
  }

  /* build the list to eval */
  object * list = the_empty_list;
  push_root(&list);
  list = cons(alien, list);
  list = cons(releaser, list);

  /* send it to interp and return the result */
  object * result = interp(list, environment);
  pop_root(&list);
  return result;
}

DEFUN1(ffi_make_cif) {
  ffi_cif * cif = MALLOC(sizeof(ffi_cif));
  return make_alien(cif, free_ptr_fn);
}

DEFUN1(ffi_primitive_type) {
  object * type = FIRST;
  ffi_type * tgt_type;
  if(type == ffi_type_pointer_sym) {
    tgt_type = &ffi_type_pointer;
  }
  else if(type == ffi_type_uint_sym) {
    tgt_type = &ffi_type_uint;
  }
  else {
    /* unknown type */
    return false;
  }

  return make_alien(tgt_type, the_empty_list);
}

DEFUN1(ffi_make_pointer_array) {
  void **array = MALLOC(sizeof(void*) * LONG(FIRST));
  return make_alien(array, free_ptr_fn);
}

DEFUN1(ffi_set_pointer) {
  void **array = ALIEN_PTR(FIRST);
  long idx = LONG(SECOND);
  void * value = ALIEN_PTR(THIRD);
  array[idx] = value;
  return FIRST;
}

DEFUN1(ffi_prep_cif_proc) {
  ffi_cif * cif = ALIEN_PTR(FIRST);
  long n_args = LONG(SECOND);
  ffi_type * rtype = ALIEN_PTR(THIRD);
  ffi_type ** args = ALIEN_PTR(FOURTH);

  if(ffi_prep_cif(cif, FFI_DEFAULT_ABI,
		  n_args, rtype, args) == FFI_OK) {
    return true;
  } else {
    return false;
  }
}

DEFUN1(ffi_call_proc) {
  ffi_cif * cif = ALIEN_PTR(FIRST);
  void (*fn)(void) = ALIEN_FN_PTR(SECOND);
  void * result = ALIEN_PTR(THIRD);
  void ** values = ALIEN_PTR(FOURTH);

  ffi_call(cif, fn, result, values);
  return true;
}

DEFUN1(ffi_value_array) {
  void ** values = MALLOC(sizeof(void*) * LONG(FIRST));
  return make_alien(values, free_ptr_fn);
}

DEFUN1(ffi_set_value) {
  void ** values = ALIEN_PTR(FIRST);
  long idx = LONG(SECOND);
  void * value = ALIEN_PTR(THIRD);

  values[idx] = value;
  return FIRST;
}

DEFUN1(ffi_address_of){
  void ** ptr = &(ALIEN_PTR(FIRST));
  return make_alien(ptr, the_empty_list);
}

typedef void(*FN_PTR)(void);

DEFUN1(puts_fn_ptr) {
  return make_alien_fn((FN_PTR)puts, the_empty_list);
}

DEFUN1(string_to_alien) {
  char * str = STRING(FIRST);
  return make_alien(strdup(str), free_ptr_fn);
}

DEFUN1(alien_to_string) {
  char * str = ALIEN_PTR(FIRST);
  return make_string(str);
}

DEFUN1(int_to_alien) {
  int * val = MALLOC(sizeof(int));
  *val = (int)LONG(FIRST);
  return make_alien(val, free_ptr_fn);
}

DEFUN1(alien_to_int) {
  int * val = ALIEN_PTR(FIRST);
  return make_fixnum(*val);
}

void init_ffi(object *env) {
#define add_procedure(scheme_name, c_name)    \
  define_variable(make_symbol(scheme_name),   \
                  curr=make_primitive_proc(c_name),	\
                  env);

  object * curr = the_empty_list;

  free_ptr_fn = make_primitive_proc(free_ptr);
  push_root(&free_ptr_fn);

  push_root(&curr);
  add_procedure("ffi-make-cif", ffi_make_cif);
  add_procedure("ffi-make-pointer-array", ffi_make_pointer_array);
  add_procedure("ffi-set-pointer!", ffi_set_pointer);
  add_procedure("ffi-primitive", ffi_primitive_type);
  add_procedure("ffi-prep-cif", ffi_prep_cif_proc);
  add_procedure("ffi-call", ffi_call_proc);
  add_procedure("ffi-free", free_ffi_alien_object);

  add_procedure("puts-fn-ptr", puts_fn_ptr);
  add_procedure("string-to-alien", string_to_alien);
  add_procedure("alien-to-string", alien_to_string);
  add_procedure("int-to-alien", int_to_alien);
  add_procedure("alien-to-int", alien_to_int);
  add_procedure("address-of", ffi_address_of);

  pop_root(&curr);

  ffi_type_pointer_sym = make_symbol("ffi-pointer");
  ffi_type_uint_sym = make_symbol("ffi-uint");
}
