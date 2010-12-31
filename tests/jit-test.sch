(require 'jit)

(set! *context* (jit:context-create))
(write "context-supports-threads"
       (jit:context-supports-threads *context*))


;; jit a function that takes two integer arguments
;; and shifts the first left by the second
(with-locked-context *context*
  (let* ((sig (jit:type-create-signature jit-int
					 (list jit-int jit-int)))
	 (func (jit:function-create *context* sig))
	 (param1 (jit:value-get-param func 0))
	 (param2 (jit:value-get-param func 1)))

    (let ((result (jit:insn-shl func param1 param2)))
      (jit:insn-return func result))

    (assert (jit:function-compile func))

    ;; give it to the world!
    (set! *func* func)))

(set! *result* (ffi:int-to-alien 0))

;; need to hang on to a reference to this alien so it
;; won't be gc'd. All aliens are opaque to gc so it won't
;; be able to find it by traversing the alien arg array.
(set! *arg-list* (list (ffi:alien-uint 42)
		       (ffi:alien-uint 1)))
(set! *args* (jit:build-arg-array *arg-list*))

(jit:function-apply *func*
		    *args*
		    (ffi:address-of *result*))

(write "the answer is" (ffi:alien-to-int *result*))

(jit:define left-shift *context*
  (jit-int (jit-int jit-int))
  jit-func
  ((param1 param2)
   (let ((res (jit:insn-shl jit-func param1 param2)))
     (jit:insn-return jit-func res)))
  ((arg1 arg2)
   (jit:binary-int-invoke jit-func arg1 arg2)))

(left-shift 1 1)
(left-shift 1 2)
(left-shift 1 3)

(map (lambda (x)
       (left-shift x 2))
     (upto 200))

(exit 0)