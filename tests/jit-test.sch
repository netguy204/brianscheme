(require 'jit)

(set! *context* (jit:context-create))
(write "context-supports-threads"
       (jit:context-supports-threads *context*))


;; jit a function that takes an integer argument and
;; simply returns it
(with-locked-context *context*
  (set! *sig* (jit:type-create-signature jit-int jit-int))

  (set! *func* (jit:function-create *context* *sig*))
  (set! *param* (jit:value-get-param *func* 0))

  (assert (jit:insn-return *func* *param*))
  (assert (jit:function-compile *func*)))

(set! *result* (ffi:int-to-alien 0))

;; need to hang on to a reference to this alien so it
;; won't be gc'd. All aliens are opaque to gc so it won't
;; be able to find it by traversing the alien arg array.
(set! *arg-list* (list (ffi:alien-uint 42)))
(set! *args* (jit:build-arg-array *arg-list*))

(jit:function-apply *func*
		    *args*
		    (ffi:address-of *result*))

(write "the answer is" (ffi:alien-to-int *result*))


