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
	 (fnspec (ffi:make-function-spec result-type
				     (map ffi:alien-type args))))

    (dotimes (idx (length args))
	     (ffi-set-pointer! values idx (nth value-ptr-list idx)))

    (ffi-call fnspec fnptr result values)

    ;; cleanup
    (let ((call-result (ffi:from-alien result result-type)))
      (map ffi-free value-ptr-list)
      (map ffi-free value-list)
      (ffi-free fnspec)
      (ffi-free result)

      call-result)))

(define (ffi:puts string)
  (ffi:funcall (puts-fn-ptr) 'ffi-uint string))

