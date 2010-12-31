(define-syntax (assert cond)
  `(begin
     (if (not ,cond)
	 (throw-error "assert failed" ',cond))))

(define (ffi:make-function-spec return args)
  (let ((cif (ffi-make-cif))
	(argspec (ffi-make-pointer-array (length args)))
	(retspec (ffi-primitive return)))

    ;; build a list of argument types
    (dotimes (idx (length args))
	     (let ((arg-type (ffi-primitive (nth args idx))))
	       (assert arg-type)
	       (ffi-set-pointer! argspec idx arg-type)))

    (assert retspec)

    ;; construct the cif
    (assert (ffi-prep-cif cif (length args) retspec argspec))
    cif))

(define (ffi:to-alien obj)
  (cond
   ((string? obj) (string-to-alien obj))
   ((integer? obj) (int-to-alien obj))
   (else (throw-error "can't convert" obj "to alien"))))

(define (ffi:from-alien obj type)
  (case type
    (ffi-uint (alien-to-int obj))
    (else (throw-error "can't convert" type "back from alien"))))

(define (ffi:alien-type obj)
  (cond
   ((string? obj) 'ffi-pointer)
   ((integer? obj) 'ffi-uint)
   (else (throw-error "don't know ffi type for" obj))))

(define (ffi:empty-alien type)
  (case type
    (ffi-uint (int-to-alien 0))
    (else (throw-error "can't make empty" type))))

(define (ffi:funcall fnptr result-type . args)
  (let* ((values (ffi-make-pointer-array (length args)))
	 (value-list (map ffi:to-alien args))
	 (value-ptr-list (map address-of value-list))
	 (result (ffi:empty-alien result-type))
	 (result-ptr (address-of result))
	 (fnspec (ffi:make-function-spec result-type
					 (map ffi:alien-type args))))

    (dotimes (idx (length args))
	     (ffi-set-pointer! values idx (nth value-ptr-list idx)))

    (ffi-call fnspec fnptr result-ptr values)

    ;; cleanup
    (let ((call-result (ffi:from-alien result result-type)))
      (map ffi-free value-ptr-list)
      (map ffi-free value-list)
      (ffi-free fnspec)
      (ffi-free result)

      call-result)))

(define-syntax (with-library handle-and-name . body)
  `(let ((,(first handle-and-name)
	  (ffi-dlopen ,(second handle-and-name))))
     (let ((result (begin . ,body)))
       (ffi-dlclose ,(first handle-and-name))
       result)))

;; simple example of using ffi to call puts
(define (ffi:puts string)
  (with-library (handle nil)
    (let ((puts (ffi-dlsym handle "puts")))
      (assert handle)
      (assert puts)

      (ffi:funcall puts 'ffi-uint string))))


(define (ffi:fork)
  (with-library (handle nil)
    (let ((fork (ffi-dlsym handle "fork")))
      (ffi:funcall fork 'ffi-uint))))

(define (ffi:sleep seconds)
  (with-library (handle nil)
    (let ((sleep (ffi-dlsym handle "sleep")))
      (ffi:funcall sleep 'ffi-uint seconds))))

(define (ffi:putchar val)
  (with-library (handle nil)
    (let ((put (ffi-dlsym handle "putchar")))
      (assert put)
      (ffi:funcall put 'ffi-uint val))))

;; this definition is a bit trickier because we don't have a
;; convenient way to express pointers to primitives.
;;
;; unsigned int wait(unsigned int* status);
;;
(define (ffi:wait)
  (with-library (handle nil)
    (let* ((wait (ffi-dlsym handle "wait"))
	   (spec (ffi:make-function-spec 'ffi-uint '(ffi-pointer)))
	   (pid (address-of (int-to-alien 0)))
	   (status (int-to-alien 0))
	   (args (ffi-make-pointer-array 1)))

      (ffi-set-pointer! args 0 (address-of (address-of status)))
      (ffi-call spec wait (address-of pid) args)

      (list (cons 'pid (alien-to-int pid))
	    (cons 'status (alien-to-int status))))))

(define (fork-test)
  (let ((pid (ffi:fork)))
    (if (= pid 0)
	(begin
	  (write "hello from the child")
	  (ffi:sleep 1)
	  (write "child is exiting")
	  (exit 1))
	(begin
	  (write "hello from parent. child is" pid)
	  (write (ffi:wait))))))




