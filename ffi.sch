(define (ffi:make-function-spec return args)
  "create callspec for a function of the given signature"
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
  "convert obj to its corresponding alien representation"
  (cond
   ((string? obj) (string-to-alien obj))
   ((integer? obj) (int-to-alien obj))
   ((alien? obj) obj)
   (else (throw-error "can't convert" obj "to alien"))))

(define (ffi:from-alien obj type)
  "convert obj from alien assuming that it's of the given type"
  (case type
    (ffi-uint (alien-to-int obj))
    (else (throw-error "can't convert" type "back from alien"))))

(define (ffi:alien-type obj)
  "determine the alien type tag for a given object"
  (cond
   ((string? obj) 'ffi-pointer)
   ((alien? obj) 'ffi-pointer)
   ((integer? obj) 'ffi-uint)
   (else (throw-error "don't know ffi type for" obj))))

(define (ffi:empty-alien type)
  "allocate default initialized alien value of a given type"
  (case type
    (ffi-uint (int-to-alien 0))
    (else (throw-error "can't make empty" type))))

(define (ffi:funcall fnptr result-type . args)
  "call alien function expecting result and using default assumed alien types for the given arguments if they're not already alien"
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
  "load a dynamic library while body is in scope"
  `(let ((,(first handle-and-name)
	  (ffi-dlopen ,(second handle-and-name))))
     (let ((result (begin . ,body)))
       (ffi-dlclose ,(first handle-and-name))
       result)))

;; simple example of using ffi to resolve symbols
;; already loaded by ld
(with-library (handle nil)
  (let ((puts (ffi-dlsym handle "puts"))
	(fork (ffi-dlsym handle "fork"))
	(sleep (ffi-dlsym handle "sleep"))
	(putchar (ffi-dlsym handle "putchar"))
	(wait (ffi-dlsym handle "wait")))

    (define (ffi:puts string)
      (ffi:funcall puts 'ffi-uint string))

    (define (ffi:fork)
      (ffi:funcall fork 'ffi-uint))

    (define (ffi:sleep seconds)
      (ffi:funcall sleep 'ffi-uint seconds))

    (define (ffi:putchar val)
      (ffi:funcall put 'ffi-uint val))

    ;; this definition is a bit trickier because we're
    ;; dealing with a pointer to a primitive
    ;;
    ;; unsigned int wait(unsigned int* status);
    ;;
    (define (ffi:wait)
      (let* ((status (int-to-alien 0))
	     (pid (ffi:funcall wait 'ffi-uint (address-of status))))

	(list (cons 'pid pid)
	      (cons 'status (alien-to-int status)))))))

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


