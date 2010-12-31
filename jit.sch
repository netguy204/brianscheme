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
       (function-apply (ffi:dlsym libjit "jit_function_apply")))

  (assert libjit)

  ;; jit type constants
  (define jit-void (ffi:deref (ffi:dlsym-var libjit "jit_type_void")))
  (define jit-int (ffi:deref (ffi:dlsym-var libjit "jit_type_int")))


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

  (define (jit:type-create-signature return . params)
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

  (define (jit:insn-not function val1 val2)
    "generate a not instruction, return the result ref"
    (ffi:funcall insn-not 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-shl function val1 val2)
    "generate a left shift instruction, return the result ref"
    (ffi:funcall insn-shl 'ffi-pointer
		 function val1 val2))

  (define (jit:insn-shr function val1 val2)
    "generate a right shift instruction, return the result ref"
    (ffi:funcall insn-shr 'ffi-pointer
		 function val1 val2))

  (define (jit:function-compile function)
    "assemble the function"
    (= (ffi:funcall function-compile 'ffi-uint function) 1))

  (define (jit:build-arg-array args)
    "use the ffi to-alien mechanism to construct a void** array"
    (ffi:values-array (ffi:make-value-array args)))

  (define (jit:function-apply function args return-area)
    "call a jit'd function with args and put the result into return-area"
    (ffi:funcall function-apply 'ffi-uint
		 function args return-area))

  (define (jit:close)
    "release the libjit library. call to free resources after no more calls will be made to jit:* methods"
    (ffi:dlclose libjit)))


(define-syntax (with-locked-context context . body)
  `(begin
     (jit:context-build-start ,context)
     (let ((result (begin . ,body)))
       (jit:context-build-end ,context)
       result)))



