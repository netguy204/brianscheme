; Copyright 2010 Brian Taylor
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;

; DESCRIPTION: Imports functions from libjit using ffi. Provides
; convenience routines for inline-assembling new primitive
; functions using libjit.

(require 'ffi)

(let* ((libjit (ffi:dlopen "libjit.so"))
       (context-create (ffi:dlsym libjit "jit_context_create"))
       (context-destroy (ffi:dlsym libjit "jit_context_destroy"))
       (context-supports-threads (ffi:dlsym
				  libjit "jit_context_supports_threads"))
       (context-build-start (ffi:dlsym libjit "jit_context_build_start"))
       (context-build-end (ffi:dlsym libjit "jit_context_build_end"))
       (type-create-signature (ffi:dlsym
			       libjit "jit_type_create_signature"))
       (function-create (ffi:dlsym libjit "jit_function_create"))
       (value-get-param (ffi:dlsym libjit "jit_value_get_param"))
       (function-compile (ffi:dlsym libjit "jit_function_compile"))
       (function-to-closure (ffi:dlsym libjit "jit_function_to_closure"))
       (insn-return (ffi:dlsym libjit "jit_insn_return"))
       (insn-add (ffi:dlsym libjit "jit_insn_add"))
       (insn-sub (ffi:dlsym libjit "jit_insn_sub"))
       (insn-mul (ffi:dlsym libjit "jit_insn_mul"))
       (insn-div (ffi:dlsym libjit "jit_insn_div"))
       (insn-rem (ffi:dlsym libjit "jit_insn_rem"))
       (insn-and (ffi:dlsym libjit "jit_insn_and"))
       (insn-or (ffi:dlsym libjit "jit_insn_or"))
       (insn-not (ffi:dlsym libjit "jit_insn_not"))
       (insn-shl (ffi:dlsym libjit "jit_insn_shl"))
       (insn-shr (ffi:dlsym libjit "jit_insn_shr"))
       (insn-eq (ffi:dlsym libjit "jit_insn_eq"))
       (insn-lt (ffi:dlsym libjit "jit_insn_lt"))
       (insn-branch-if-not
	(ffi:dlsym libjit "jit_insn_branch_if_not"))
       (insn-load (ffi:dlsym libjit "jit_insn_load"))
       (insn-load-relative (ffi:dlsym libjit "jit_insn_load_relative"))
       (value-create-nint
	(ffi:dlsym libjit "jit_value_create_nint_constant"))
       (value-create-long
	(ffi:dlsym libjit "jit_value_create_long_constant"))
       (insn-label (ffi:dlsym libjit "jit_insn_label"))
       (insn-call (ffi:dlsym libjit "jit_insn_call"))
       (insn-call-native (ffi:dlsym libjit "jit_insn_call_native"))
       (dump-function (ffi:dlsym libjit "jit_dump_function"))
       (function-apply (ffi:dlsym libjit "jit_function_apply")))

  (assert libjit)

  ;; jit type constants
  (define jit-void
    (ffi:deref (ffi:dlsym-var libjit "jit_type_void")))
  (define jit-short
    (ffi:deref (ffi:dlsym-var libjit "jit_type_short")))
  (define jit-int
    (ffi:deref (ffi:dlsym-var libjit "jit_type_int")))
  (define jit-long
    (ffi:deref (ffi:dlsym-var libjit "jit_type_long")))
  (define jit-ulong
    (ffi:deref (ffi:dlsym-var libjit "jit_type_ulong")))
  (define jit-void-ptr
    (ffi:deref (ffi:dlsym-var libjit "jit_type_void_ptr")))

  (define (jit:context-create)
    "create an opaque jit context"
    (ffi:funcall context-create 'ffi-pointer))

  (define (jit:context-destroy ctx)
    "destroy the given jit context"
    (ffi:funcall context-destroy 'ffi-void ctx))

  (define (jit:context-supports-threads ctx)
    "true if the provided context supports threads. note that the interpreter and VM DO NOT support threads so you shouldn't use them from here."
    (= (ffi:funcall context-supports-threads 'ffi-uint ctx) 1))

  (define (jit:context-build-start ctx)
    "called to mark the beginning of a jit"
    (ffi:funcall context-build-start 'ffi-void ctx))

  (define (jit:context-build-end ctx)
    "called to mark the end of a jit"
    (ffi:funcall context-build-end 'ffi-void ctx))

  (define-struct jit:params
    "parameters to a jit'd function"
    ((array)
     (length)))

  (define (jit:make-parameter-signature types)
    "create a signature for the arguments of a function"
    (let ((array (ffi:make-pointer-array (length types))))
      (dotimes (idx (length types))
	(ffi:set-array-pointer! array idx (nth types idx)))

      (make-jit:params 'array array
		       'length (length types))))

  (define (jit:type-create-signature return params)
    "create a full function signature"
    (let ((parm-sig (jit:make-parameter-signature params)))
      (ffi:funcall type-create-signature
		   'ffi-pointer ;; returns jit_type_t
		   (ffi:alien-uint 0) ;; jit_abi_cdecl
		   return
		   (jit:params-array parm-sig)
		   (jit:params-length parm-sig)
		   (ffi:alien-uint 1)))) ;; incref

  (define (jit:function-create ctx signature)
    "create a new function object"
    (ffi:funcall function-create 'ffi-pointer
		 ctx signature))

  (define (jit:value-get-param function idx)
    "get a reference to a function's parameter"
    (ffi:funcall value-get-param 'ffi-pointer
		 function (ffi:alien-uint idx)))

  (define (jit:insn-return function value)
    "generate a return instruction from function with value"
    (= (ffi:funcall insn-return 'ffi-uint
		    function value) 1))

  (define (jit:insn-add function val1 val2)
    "generate an add instruction, return the result ref"
    (ffi:funcall insn-add 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-sub function val1 val2)
    "generate a subtract instruction, return the result ref"
    (ffi:funcall insn-sub 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-mul function val1 val2)
    "generate a multiply instruction, return the result ref"
    (ffi:funcall insn-mul 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-div function val1 val2)
    "generate a divide instruction, return the result ref"
    (ffi:funcall insn-div 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-rem function val1 val2)
    "generate a modulo instruction, return the result ref"
    (ffi:funcall insn-rem 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-and function val1 val2)
    "generate an and instruction, return the result ref"
    (ffi:funcall insn-and 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-or function val1 val2)
    "generate an or instruction, return the result ref"
    (ffi:funcall insn-or 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-not function val1)
    "generate a not instruction, return the result ref"
    (ffi:funcall insn-not 'ffi-pointer
		 function val1))

  (define (jit:insn-shl function val1 val2)
    "generate a left shift instruction, return the result ref"
    (ffi:funcall insn-shl 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-shr function val1 val2)
    "generate a right shift instruction, return the result ref"
    (ffi:funcall insn-shr 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-eq function val1 val2)
    "generate a comparison, returns result ref"
    (ffi:funcall insn-eq 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-lt function val1 val2)
    "generate a comparison, returns result ref"
    (ffi:funcall insn-lt 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-branch-if-not function cond label)
    "branch to label if cond is false"
    (ffi:funcall insn-branch-if-not 'ffi-uint
		 function cond (ffi:address-of label)))

  (define (jit:insn-label function label)
    "insert a label into the function"
    (ffi:funcall insn-label 'ffi-uint
		 function (ffi:address-of label)))

  (define (jit:insn-load function value)
    "dereference value. returns result ref"
    (ffi:funcall insn-load 'ffi-pointer
		 function value))

  (define (jit:insn-load-relative
	   function base offset type)
    "load value of type, offset steps from base. returns result ref"
    (ffi:funcall insn-load-relative 'ffi-pointer
		 function base (ffi:alien-uint offset) type))

  (define (jit:value-create-nint-constant function type val)
    "create a constant. returns result ref"
    (ffi:funcall value-create-nint 'ffi-pointer
		 function type val))

  (define (jit:value-create-long-constant function type val)
    "create a constant. returns result ref"
    (ffi:funcall value-create-long 'ffi-pointer
		 function type val))

  (define (jit:insn-call
	   function name target args)
    "call the jit'd function target with arguments, returns result ref"
    (let ((array (ffi:make-pointer-array (length args))))
      (dotimes (idx (length args))
        (ffi:set-array-pointer! array idx (nth args idx)))

      (ffi:funcall insn-call 'ffi-pointer
		   function name target
		   (ffi:int-to-alien 0) ; signature, null works?
		   array
		   (ffi:alien-uint (length args))
		   (ffi:int-to-alien 0)))) ; flags, none.

  (define (jit:insn-call-native
	   function name fn-ptr sig args)
    "call the native function with arguments. returns result ref"
    (let ((array (ffi:make-pointer-array (length args))))
      (dotimes (idx (length args))
        (ffi:set-array-pointer! array idx (nth args idx)))

      (ffi:funcall insn-call-native 'ffi-pointer
		   function name fn-ptr sig
		   array (ffi:alien-uint (length args))
		   (ffi:int-to-alien 0)))) ; flags, none

  (define (jit:dump-function stream function name)
    "dump a function to a stream"
    (ffi:funcall dump-function 'ffi-void
		 (ffi:stream-to-alien stream)
		 function
		 (ffi:alien-string name)))

  (define (jit:function-compile function)
    "assemble the function"
    (= (ffi:funcall function-compile 'ffi-uint function) 1))

  ; FIXME: This leaks memory
  (define (jit:build-arg-array args)
    "use the ffi to-alien mechanism to construct a void** array"
    (ffi:values-array (ffi:make-value-array args)))

  (define (jit:function-apply function args return-area)
    "call a jit'd function with args and put the result into return-area"
    (ffi:funcall function-apply 'ffi-uint
		 function args return-area))

  (define (jit:function-to-closure function)
    "get a function pointer to a jit'd function"
    (ffi:funcall function-to-closure 'ffi-pointer
		 function))

  (define (jit:close)
    "release the libjit library. call to free resources after no more calls will be made to jit:* methods"
    (ffi:dlclose libjit)))

;; create a global context because we'll always need one
(set! *context* (jit:context-create))

(define-syntax (with-locked-context context . body)
  "locks jit context while body executes"
  `(begin
     (jit:context-build-start ,context)
     (let ((result (begin . ,body)))
       (jit:context-build-end ,context)
       result)))

(define (jit:expand-symbol sym)
  "internal: expands a concise op symbol into the fully qualified form"
  (string->symbol
   (concat "jit:insn-"
	   (symbol->string sym))))

(define (jit:expand-opcode fn op)
  "internal: expands a concise opcode into the fully qualified form"
  `(assert (,(jit:expand-symbol (first op))
	    ,fn . ,(rest op))))


(define-syntax (jit:assemble fn . ops)
  "enables writing assembly in a bit more consise format"
  (cond
   ((null? ops) fn)
   ((symbol? (first ops)) (first ops))
   (else
    `(let (,(if (length=1 (first ops))
		`(,(gensym) ,(jit:expand-opcode fn (first (first ops))))
		`(,(first (first ops))
		  ,(jit:expand-opcode fn (second (first ops))))))
       (jit:assemble ,fn . ,(rest ops))))))

(define-syntax (jit:define
		 name context sig fn fn-ptr
		 args-and-insts . maybe-args-and-body)
  "compiles an instruction stream into a jit'd function and accepts optional frontend code to handle boxing and unboxing"
  (let ((compiled-sig (gensym)))
    `(with-locked-context ,context
       ;; create the signature and the function
       (let* ((,compiled-sig (jit:type-create-signature
			      ,(first sig)
			      (list . ,(second sig))))
	      (,fn (jit:function-create ,context
					,compiled-sig)))

	 ;; bringing the requested jit args into view
	 ;; for defining the assembly stream
	 (let ,(map (lambda (idx)
		      (list (nth (car args-and-insts) idx)
			    `(jit:value-get-param ,fn ,idx)))
		    (upto (length (car args-and-insts))))

	   . ,(rest args-and-insts))

	 (assert (jit:function-compile ,fn))

	 ;; now the userspace callsite definition
	 (let ((,fn-ptr (jit:function-to-closure ,fn)))
	   ,(if (null? maybe-args-and-body)
		`(set! ,name (ffi:alien-to-primitive-proc ,fn-ptr))
		(let ((args-and-body (car maybe-args-and-body)))
		  `(define (,name . ,(first args-and-body))
		     . ,(rest args-and-body)))))))))


(define (jit:binary-int-invoke jit-func arg1 arg2)
  "invoke a binary jit'd function of two ints that returns an int"
  (ffi:funcall jit-func 'ffi-uint arg1 arg2))

(define (jit:unary-int-invoke jit-func arg1)
  "invoke a unary jit'd function of an int that returns an int"
  (ffi:funcall jit-func 'ffi-uint arg1))

(define (jit:make-label)
  (ffi:int-to-alien 4294967295))

;; setup symbols for manipulating objects
(with-library (lib nil)
  (let ((make-fixnum (ffi:dlsym lib "make_fixnum"))
	(make-fixnum-sig
	 (jit:type-create-signature jit-void-ptr
				    (list jit-long))))

    (define jit:fixnum-offset
      (ffi:alien-to-int
       (ffi:deref (ffi:dlsym-var lib "fixnum_offset"))))

    (define jit:car-offset
      (ffi:alien-to-int
       (ffi:deref (ffi:dlsym-var lib "car_offset"))))

    (define jit:cdr-offset
      (ffi:alien-to-int
       (ffi:deref (ffi:dlsym-var lib "cdr_offset"))))

    (define (jit:insn-const-long fn val)
      "defines a constant long. returns result ref"
      (jit:value-create-long-constant fn jit-long val))

    (define (jit:insn-const-ulong fn val)
      "defines a constant unsigned long. returns result ref"
      (jit:value-create-long-constant fn jit-ulong val))

    (define (jit:insn-car fn obj)
      "gets the car of obj and returns result ref"
      (jit:assemble
       fn
       (val (load-relative obj jit:car-offset jit-void-ptr))
       val))

    (define (jit:insn-cdr fn obj)
      "gets the cdr of obj and returns result ref"
      (jit:assemble
       fn
       (val (load-relative obj jit:cdr-offset jit-void-ptr))
       val))

    (define jit:insn-first jit:insn-car)

    (define (jit:insn-cadr fn obj)
      "get the cadr of obj and return result ref"
      (jit:assemble
       fn
       (v1 (cdr obj))
       (val (car v1))
       val))

    (define jit:insn-second jit:insn-cadr)

    (define (jit:insn-unbox-long fn boxed)
      "unboxes value as fixnum and returns result ref"
      (jit:assemble
       fn
       (val (load-relative boxed jit:fixnum-offset jit-long))
       val))

    (define (jit:insn-box-long fn val)
      "boxes value as fixnum and returns result ref"
      (jit:insn-call-native fn
			    "make_fixnum"
			    make-fixnum
			    make-fixnum-sig
			    (list val)))))


;; now lets define some stuff we couldn't have had before
;; using only the language primitives
;; we sorta need binary-not to make fresh labels
(jit:define binary-not *context*
  (jit-void-ptr (jit-void-ptr jit-void-ptr))
  fn fn-ptr
  ((args env)
   (jit:assemble
    fn
    (boxed-num (first args))
    (val (unbox-long boxed-num))
    (result (not val))
    (boxed-res (box-long result))
    ((return boxed-res)))))

(define-syntax (jit:assemble-2long name v1 v2 doc . body)
  "create a function of to long arguments"
  (add-documentation name doc)
  (let ((args (gensym))
	(env (gensym))
	(fn (gensym))
	(fn-ptr (gensym))
	(b1 (gensym))
	(b2 (gensym)))

    `(jit:define ,name *context*
       (jit-void-ptr (jit-void-ptr jit-void-ptr))
       ,fn ,fn-ptr
       ((,args ,env)
	(jit:assemble
	 ,fn
	 (,b1 (first ,args))
	 (,b2 (second ,args))
	 (,v1 (unbox-long ,b1))
	 (,v2 (unbox-long ,b2))
	 . ,body)))))

(jit:assemble-2long binary-shl v1 v2
  "binary shift v1 to the left by v2"
  (result (shl v1 v2))
  (box (box-long result))
  ((return box)))

(jit:assemble-2long binary-shr v1 v2
  "binary shift v1 to the right by v2"
  (result (shr v1 v2))
  (box (box-long result))
  ((return box)))

(jit:assemble-2long binary-and v1 v2
  "compute the bitwise and of v1 and v2"
  (result (and v1 v2))
  (box (box-long result))
  ((return box)))

(jit:assemble-2long binary-or v1 v2
  "compute the bitwise or of v1 and v2"
  (result (or v1 v2))
  (box (box-long result))
  ((return box)))
