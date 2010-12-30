(define-syntax (assert cond)
  `(begin
     (if (not ,cond)
	 (throw-error "assert failed" cond))))

(define (function:ptr->int)
  (let ((cif (ffi-make-cif))
	(args (ffi-make-pointer-array 1)))

    (ffi-set-pointer! args 0 (ffi-primitive 'ffi-pointer))
    (assert (ffi-prep-cif cif 1
			  (ffi-primitive 'ffi-uint)
			  args))

    cif))

(define (ffi:puts string)
  (let* ((values (ffi-make-pointer-array 1))
	 (msg (string-to-alien string))
	 (msg-ptr (address-of msg))
	 (fn-desc (function:ptr->int))
	 (fn (puts-fn-ptr))
	 (result (int-to-alien 0)))

    (ffi-set-pointer! values 0 msg-ptr)
    (ffi-call fn-desc fn result values)

    (ffi-free msg)
    (ffi-free values)
    (ffi-free fn-desc)

    (let ((result-int (alien-to-int result)))
      (ffi-free result)
      result-int)))

