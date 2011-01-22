(require 'unittest)
(require 'ffi)

(define-test (ffi-test)
  (check
   ;; automatic type conversion
   (instance-of? <ffi:alien-uint> (ffi:to-alien 22))
   (instance-of? <ffi:alien-uchar> (ffi:to-alien #\c))
   (instance-of? <ffi:alien-string> (ffi:to-alien "foo"))
   (instance-of? <ffi:alien-pointer> (ffi:address-of
				      (ffi:to-alien 1)))
   (instance-of? <ffi:alien-pointer-array>
		 (ffi:make-value-array '(1 2 "foo")))

   ;; converting an ffi:alien does nothing
   (instance-of? <ffi:alien-uint> (ffi:to-alien
				   (ffi:to-alien 22)))

   (eq? 'ffi-pointer (ffi:alien-ffi-type
		      (ffi:address-of (ffi:to-alien 1))))


   ;; extracting the real alien value
   (instance-of? <alien> (ffi:alien-value (ffi:to-alien 1)))
   (instance-of? <alien> (ffi:alien-value
			  (ffi:address-of (ffi:to-alien 1))))

   ;; pointers and type conversion
   (= 1 (ffi:from-alien
	 (ffi:deref (ffi:address-of (ffi:to-alien 1)))))

   (eq? "foo"
	(ffi:from-alien
	 (ffi:deref (ffi:address-of (ffi:to-alien "foo")))))

   (eq? #\c
	(ffi:from-alien
	 (ffi:deref (ffi:address-of (ffi:to-alien #\c)))))

   ;; pointers with gc
   (= 1 (let ((ptr (ffi:address-of (ffi:to-alien 1))))
	  (mark-and-sweep)
	  (ffi:from-alien (ffi:deref ptr)))))

  ;; pointer arrays
  (let ((pa (ffi:make-value-array '(1 2 "foo"))))
    ;; clear out any dangling objects
    (mark-and-sweep)
    ;; and make a lot of garbage
    (upto 200)
    ;; verify the values were protected
    (check
     (= 1 (ffi:from-alien (ffi:value-array-original-deref pa 0)))
     (= 2 (ffi:from-alien (ffi:value-array-original-deref pa 1)))
     (eq? "foo" (ffi:from-alien (ffi:value-array-original-deref pa 2)))))


  ;; build a cif
  (let* ((pa (ffi:make-value-array '(1 "foo")))
	 (result (ffi:empty-alien 'ffi-uint))
	 (cif (ffi:make-function-spec 'ffi-uint
				      (ffi:value-array-types pa))))

    (check
     (equal? '(ffi-uint ffi-pointer)
	     (ffi:value-array-types pa))

     (instance-of? <ffi:cif> cif)
     (instance-of? <alien> (slot-ref cif 'cif))
     (ffi:free cif)
     (ffi:free pa)))


  ;; multibyte values
  (check
   (instance-of? <ffi:multibyte-raw-pointer>
		 (ffi:address-of (make <ffi:multibyte-raw-value>
				   'length 16)))

   (instance-of? <ffi:multibyte-raw-value>
		 (ffi:deref (make <ffi:multibyte-raw-pointer>
			      'length 16)))))




