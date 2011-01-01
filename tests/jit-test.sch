(require 'jit)

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

;; try converting the opaque function handle into a function
;; pointer
(set! *fptr* (jit:function-to-closure *func*))

;; now invoke it again
(write "the answer is now" (ffi:funcall *fptr* 'ffi-uint 42 1))

(jit:define left-shift *context*
  (jit-int (jit-int jit-int))
  jit-func fn-ptr
  ((param1 param2)
   (let ((res (jit:insn-shl jit-func param1 param2)))
     (jit:insn-return jit-func res)))
  ((arg1 arg2)
   (jit:binary-int-invoke fn-ptr arg1 arg2)))

(left-shift 1 1)
(left-shift 1 2)
(left-shift 1 3)

;; define a recursive function
(jit:define my-gcd *context*
  (jit-int (jit-int jit-int))
  fn fn-ptr
  ((a b)

   (let*
       ((l1 (jit:make-label))
	(l2 (jit:make-label))

	;; if a == b: return a
	(a=b? (jit:insn-eq fn a b))
	(t1 (jit:insn-branch-if-not fn a=b? l1))
	(t2 (jit:insn-return fn a))
	(t3 (jit:insn-label fn l1))

	;; if a < b: gcd(a, b-a)
	(a<b? (jit:insn-lt fn a b))
	(t4 (jit:insn-branch-if-not fn a<b? l2))

	(b-a (jit:insn-sub fn b a))
	(r1 (jit:insn-call fn "my_gcd" fn
			   (list a b-a)))
	(t5 (jit:insn-return fn r1))

	(t6 (jit:insn-label fn l2))

	;; return gcd(a-b, b)
	(a-b (jit:insn-sub fn a b))
	(r2 (jit:insn-call fn "my_gcd" fn
			   (list a-b b)))
	(t7 (jit:insn-return fn r2)))
     (set! *my-gcd* fn)))

  ((a b)
   "compute the greatest common divisor of two numbers"
   (jit:binary-int-invoke fn-ptr a b)))

(jit:dump-function stdout *my-gcd* "func")

;; define a primitive function
(jit:define unit *context*
  (jit-void-ptr (jit-void-ptr jit-void-ptr))
  fn fn-ptr
  ((args env)
   (jit:insn-return fn args)))

(unit 123)

