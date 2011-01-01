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
   (jit:assemble
    jit-func
    (res (shl param1 param2))
    ((return res))))

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
   (let ((l1 (jit:make-label))
	 (l2 (jit:make-label)))
     (jit:assemble
      fn
      (a=b? (eq a b))
      ((branch-if-not a=b? l1))
      ((return a))
      ((label l1))
      (a<b? (lt a b))
      ((branch-if-not a<b? l2))
      (b-a (sub b a))
      (r1 (call "my_gcd" fn (list a b-a)))
      ((return r1))
      ((label l2))
      (a-b (sub a b))
      (r2 (call "my_gcd" fn (list a-b b)))
      ((return r2))))

   (set! *my-gcd* fn))

  ;; need a userspace invoker for this one
  ((a b)
   "compute the greatest common divisor of two numbers"
   (jit:binary-int-invoke fn-ptr a b)))

(jit:dump-function stdout *my-gcd* "func")

;; define a primitive function, has the classic interpeted
;; function signature. don't need a userspace invoker
(jit:define unit *context*
  (jit-void-ptr (jit-void-ptr jit-void-ptr))
  fn fn-ptr
  ((args env)
   (jit:insn-return fn args)))

(unit 123)

(jit:define one *context*
  (jit-void-ptr (jit-void-ptr jit-void-ptr))
  fn fn-ptr
  ((args env)
   (jit:assemble
    fn
    (const (const-long 1))
    (boxed (box-long const))
    ((return boxed)))))

(one)

jit:fixnum-offset
jit:car-offset
jit:cdr-offset

(jit:define my-car *context*
  (jit-void-ptr (jit-void-ptr jit-void-ptr))
  fn fn-ptr
  ((args env)
   (jit:assemble
    fn
    (c1 (car args)) ;; first argument
    (c2 (car c1)) ;; first in that list
    ((return c2)))
   (set! *mycar* fn)))

(jit:define my-cdr *context*
  (jit-void-ptr (jit-void-ptr jit-void-ptr))
  fn fn-ptr
  ((args env)
   (jit:assemble
    fn
    (c1 (car args)) ;; first argument
    (c2 (cdr c1)) ;; rest of that list
    ((return c2)))
   (set! *mycar* fn)))

(jit:define plus1 *context*
  (jit-void-ptr (jit-void-ptr jit-void-ptr))
  fn fn-ptr
  ((args env)
   fn
   (jit:assemble
    fn
    (arg (car args)) ;; first argument
    (num (unbox-long arg))
    (one (const-long 1))
    (sum (add num one)) ;; add them
    (box (box-long sum))
    ((return box)))))

(jit:dump-function stdout *mycar* "my_car")

(my-car '(1 2))


