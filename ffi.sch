; Copyright 2010 Brian Taylor
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;

; DESCRIPTION: Provides the userspace interface to the native
; ffi library symbols exported by ffi.c
;

(define-class <ffi:cif> ()
  "internal representation of cif. holds onto stuff that needs to be
freed later."
  ('cif
   'argspec
   'retspec
   'fn-ptr))

(let ((native-free ffi:free))
  (define-generic ffi:free
    "free the memory associated with an ffi object")

  (define-method (ffi:free (obj <alien>))
    (native-free obj)))

(define-method (ffi:free (obj <ffi:cif>))
  (ffi:free (slot-ref obj 'cif))
  (ffi:free (slot-ref obj 'argspec))
  (ffi:free (slot-ref obj 'retspec))
  #t)

(define (ffi:make-function-spec return args)
  "create callspec for a function of the given signature"
  (let ((cif (ffi:make-cif))
	(argspec (ffi:make-pointer-array (length args)))
	(retspec (ffi:primitive return)))

    (dolist-idx ((arg idx) args)
      (let ((arg-type (assert (ffi:primitive arg))))
	(ffi:set-array-pointer! argspec idx arg-type)))
    (assert retspec)

    ;; construct the cif
    (assert (ffi:prep-cif cif (length args) retspec argspec))

    (make <ffi:cif>
      'cif cif
      'argspec argspec
      'retspec retspec)))

(define-class <ffi:alien> ()
  "a wrapped alien type"
  ('value))

(define (ffi:alien-value alien)
  (slot-ref alien 'value))

(define-method (ffi:free (obj <ffi:alien>))
  (ffi:free (ffi:alien-value obj)))

(define-generic ffi:alien-ffi-type
  "return the type recognized by ffi for this wrapped type")

(define-generic ffi:to-alien
  "convert a native type into its <ffi:alien> equivalent")

(define-generic ffi:from-alien
  "convert a wrapped alien back to its native type")

(define-method (ffi:to-alien (obj <ffi:alien>))
  obj)

(define-method (ffi:from-alien (obj <ffi:alien>))
  obj)

;;(define-method (ffi:alien-ffi-type (obj <alien>))
;;  'ffi-pointer)

;;
;; strings
;;
(define-class <ffi:alien-string> (<ffi:alien>)
  "wrapped alien string")

(define (ffi:alien-string str)
  "create an alien string"
  (make <ffi:alien-string>
    'value (ffi:string-to-alien str)))

(define-method (ffi:alien-ffi-type (obj <ffi:alien-string>))
  'ffi-pointer)

(define-method (ffi:to-alien (obj <string>))
  (ffi:alien-string obj))

(define-method (ffi:from-alien (obj <ffi:alien-string>))
  (ffi:alien-to-string (ffi:alien-value obj)))

;;
;; characters
;;
(define-class <ffi:alien-uchar> (<ffi:alien>)
  "unsigned character")

(define (ffi:alien-uchar val)
  "create an alien unsigned char"
  (make <ffi:alien-uchar>
    'value (ffi:int-to-alien val)))

(define-method (ffi:alien-ffi-type (obj <ffi:alien-uchar>))
  'ffi-uchar)

(define-method (ffi:to-alien (obj <char>))
  (ffi:alien-uchar (char->integer obj)))

(define-method (ffi:from-alien (obj <ffi:alien-uchar>))
  (integer->char (ffi:alien-to-int (ffi:alien-value obj))))

;;
;; shorts
;;
(define-class <ffi:alien-ushort> (<ffi:alien>)
  "unsigned short")

(define (ffi:alien-ushort val)
  "create an alien unsigned short"
  (make <ffi:alien-ushort>
    'value (ffi:int-to-alien val)))

(define-method (ffi:alien-ffi-type (obj <ffi:alien-ushort>))
  'ffi-ushort)

(define-method (ffi:from-alien (obj <ffi:alien-ushort>))
  (ffi:alien-to-int (ffi:alien-value obj)))

;;
;; machine integers
;;
(define-class <ffi:alien-uint> (<ffi:alien>)
  "machine sized unsigned integer")

(define (ffi:alien-uint val)
  "create an alien unsigned int"
  (make <ffi:alien-uint>
    'value (ffi:int-to-alien val)))

(define-method (ffi:alien-ffi-type (obj <ffi:alien-uint>))
  'ffi-uint)

(define-method (ffi:to-alien (obj <integer>))
  (ffi:alien-uint obj))

(define-method (ffi:from-alien (obj <ffi:alien-uint>))
  (ffi:alien-to-int (ffi:alien-value obj)))

;;
;; opaques
;;
(define-class <ffi:alien-opaque> (<ffi:alien>)
  "an opaque alien object of some kind")

(define-method (ffi:to-alien (obj <alien>))
  (make <ffi:alien-opaque>
    'value obj))

(define-method (ffi:alien-ffi-type (obj <ffi:alien-opaque>))
  'ffi-pointer)


;;
;; non-opaque pointers
;;
(define-class <ffi:alien-pointer> (<ffi:alien>)
  "types pointer to an alien"
  ('target-type
   'original-target)) ;; save this to protect it from the garbage
		     ;; collector

(define-method (ffi:alien-ffi-type (obj <ffi:alien-pointer>))
  'ffi-pointer)

(define-method (ffi:from-alien (obj <ffi:alien-pointer>))
  obj)

(let ((native-address-of ffi:address-of)
      (native-deref ffi:deref))

  (define-generic ffi:address-of
    "returns a pointer to a given alien")

  (define-generic ffi:deref
    "dereferences a pointer and returns the alien result")

  ;; autopromote <alien> to <ffi:alien-opaque> and then do the
  ;; address-of
  (define-method (ffi:address-of (obj <alien>))
    (ffi:address-of (ffi:to-alien obj)))

  (define-method (ffi:address-of (obj <ffi:alien>))
    (make <ffi:alien-pointer>
      'target-type (list (class-of obj))
      'original-target (list obj)
      'value (native-address-of (ffi:alien-value obj))))

  (define-method (ffi:address-of (obj <ffi:alien-pointer>))
    (make <ffi:alien-pointer>
      'target-type (cons (class-of obj) (slot-ref obj 'target-type))
      'original-target (cons obj (slot-ref obj 'original-target))
      'value (native-address-of (ffi:alien-value obj))))

  ;; dereference the <alien> and then autopromote to
  ;; <ffi:alien-opaque>
  (define-method (ffi:deref (obj <alien>))
    (ffi:to-alien (native-deref obj)))

  ;; if it's not a typed pointer we'll assume the dereferenced value
  ;; is opaque
  (define-method (ffi:deref (obj <ffi:alien>))
    (ffi:to-alien (native-deref (ffi:alien-value obj))))

  (define-method (ffi:deref (obj <ffi:alien-pointer>))
    (if (eq? (car (slot-ref obj 'target-type)) <ffi:alien-pointer>)
	;; target is a pointer
	(make <ffi:alien-pointer>
	  'target-type (cdr (slot-ref obj 'target-type))
	  'original-target (cdr (slot-ref obj 'original-target))
	  'value (native-deref (ffi:alien-value obj)))
	;; target is not a pointer
	(make (car (slot-ref obj 'target-type))
	  'value (native-deref (ffi:alien-value obj))))))


;;
;; void needs a type
;;
(define-class <ffi:void> (<ffi:alien>)
  "void class")

(define-method (ffi:alien-ffi-type (obj <ffi:void>))
  'ffi-void)

(define-method (initialize (obj <ffi:void>) args)
  (slot-set! obj 'value (ffi:int-to-alien 0)))

;;
;; pointer arrays
;;
(define-class <ffi:alien-pointer-array> (<ffi:alien>)
  "an array of alien values as void**"
  ('ptr-list))  ;; the pointers to the values (this is whats actually
		;; in the array) are kept here to protect them from
		;; the gc. they, in turn protect the values they point
		;; to

(define-method (ffi:free (values <ffi:alien-pointer-array>))
  (call-next-method)

  ;; dereference to the stored value and free that
  (for-each (lambda (val)
	      (ffi:free (ffi:deref val)))
	    (slot-ref values 'ptr-list))

  (for-each ffi:free (slot-ref values 'ptr-list)))

(define (ffi:make-value-array args)
  "convert a list of scheme and alien arguments into a void**"
  (let* ((values (ffi:make-pointer-array (length args)))
	 (value-list (map ffi:to-alien args))
	 (value-ptr-list (map ffi:address-of value-list)))

    (dolist-idx ((val idx) value-ptr-list)
      (ffi:set-array-pointer! values idx (ffi:alien-value val)))

    (make <ffi:alien-pointer-array>
      'value values
      'ptr-list value-ptr-list)))

(define (ffi:value-array-types varray)
  "get the types of the values in the array"
  (map (lambda (v)
	 (ffi:alien-ffi-type (first (slot-ref v 'original-target))))
       (slot-ref varray 'ptr-list)))

(define (ffi:value-array-original-deref array idx)
  "return the original alien form created and placed in array"
  (ffi:deref (list-ref
	      (slot-ref array 'ptr-list) idx)))


;;
;; multi-byte raw value/pointer
;;

;; the value form of multibyte won't fit in an alien so it's not an
;; alien. you have to find its address to get an alien
(define-class <ffi:multibyte-raw-value> ()
  "a value is some number of bytes large"
  ('bytes
   'length
   'type))

(define-method (initialize (mrv <ffi:multibyte-raw-value>) args)
  (let ((nbytes (getl args 'length nil))
	(type (getl args 'type 'ffi-pointer))
	(bytes (getl args 'bytes nil)))

    (slot-set! mrv 'bytes (if bytes bytes
			      (ffi:make-bytes nbytes)))
    (slot-set! mrv 'length nbytes)
    (slot-set! mrv 'type type)))

(define-method (ffi:alien-ffi-type (mrv <ffi:multibyte-raw-value>))
  (slot-ref mrv 'type))

(define-class <ffi:multibyte-raw-pointer> (<ffi:alien>)
  "a pointer to a value that is some number of byte large"
  ('length
   'target-type))

(define-method (initialize (mrp <ffi:multibyte-raw-pointer>) args)
  (let ((nbytes (getl args 'length nil))
	(target-type (getl args 'target-type 'ffi-pointer))
	(value (getl args 'value nil)))

    (slot-set! mrp 'value (if value value
			      (ffi:make-bytes nbytes)))
    (slot-set! mrp 'length nbytes)
    (slot-set! mrp 'target-type target-type)))

(define-method (ffi:alien-ffi-type (mrv <ffi:multibyte-raw-pointer>))
  'ffi-pointer)

(define-method (ffi:address-of (mrv <ffi:multibyte-raw-value>))
  (make <ffi:multibyte-raw-pointer>
    'value (slot-ref mrv 'bytes)
    'length (slot-ref mrv 'length)
    'target-type (ffi:alien-ffi-type mrv)))

(define-method (ffi:deref (mrp <ffi:multibyte-raw-pointer>))
  (make <ffi:multibyte-raw-value>
    'length (slot-ref mrp 'length)
    'type (slot-ref mrp 'target-type)
    'bytes (ffi:alien-value mrp)))

(define-method (ffi:from-alien (mrv <ffi:multibyte-raw-value>))
  mrv)

;; by default, don't free mrv's since they may actually be the return
;; value the user wants
(define-method (ffi:free (mrv <ffi:multibyte-raw-value>))
  nil)

;; this lets you do it for real
(define (ffi:free-mrv mrv)
  "Actually free an mrv. by default, ffi:free won't work on
mrv's. They aren't really aliens after all--more like unpacked
natives."
  (ffi:free (slot-ref mrv 'bytes)))

(define-generic ffi:to-bytearray
  "convert a multibyte value into an array of characters")

(define-method (ffi:to-bytearray (mrv <ffi:multibyte-raw-value>))
  (let ((result nil)
	(bytes (slot-ref mrv 'bytes)))

    (dotimes (idx (slot-ref mrv 'length))
      (push! (char->integer (ffi:byte-ref bytes idx)) result))

    (reverse result)))

(define-method (ffi:to-bytearray (mrp <ffi:multibyte-raw-pointer>))
  (ffi:to-bytearray (ffi:deref mrp)))

(define-class <ffi:uint64> (<ffi:multibyte-raw-value>)
  "portable representation of a uint64")

(define-method (initialize (obj <ffi:uint64>))
  ;; don't like having to do this. having slot initializers in the
  ;; class definition would be a better solution
  (slot-set! obj 'length 8)
  (slot-set! obj 'bytes (ffi:make-bytes 8))
  (slot-set! obj 'type 'ffi-uint64))

(define-method (ffi:from-alien (obj <ffi:uint64>))
  ;; todo
  obj)

;;
;; convenience overrides
;;
(let ((native-atos ffi:alien-to-string)
      (native-atoi ffi:alien-to-int))
  (define-generic ffi:alien-to-string
    "convert an alien value into a string")

  (define-method (ffi:alien-to-string (obj <alien>))
    (native-atos obj))

  (define-method (ffi:alien-to-string (obj <ffi:alien-pointer>))
    (native-atos (ffi:alien-value obj)))

  (define-generic ffi:alien-to-int
    "convert an alien value into a fixnum")

  (define-method (ffi:alien-to-int (obj <alien>))
    (native-atoi obj))

  (define-method (ffi:alien-to-int (obj <ffi:alien>))
    (native-atoi (ffi:alien-value obj))))

(define (ffi:empty-alien type)
  "pre-allocate space for the return type if its a basic
primitive. for non-primitives the user is expected to provide a
pre-allocated instance of some kind of <ffi:alien> that's appropriate
for holding this"
  (cond
   ((instance-of? <ffi:alien> type) type)
   ((instance-of? <ffi:multibyte-raw-value> type) type)
   (else
    (case type
      (ffi-uint (ffi:to-alien 0))
      (ffi-pointer (ffi:address-of
		    (make <ffi:alien>
		      'value (ffi:int-to-alien 0))))
      (ffi-void (make <ffi:void>))
      (else (throw-error "don't know how to make empty" type))))))

(define-syntax (ffi:funcall fnptr result-type . args)
  "call alien function expecting result and using default assumed
alien types for the given arguments if they're not already alien. This
macro may mutate fnptr to cache its function signature"
  `(let* ((values (ffi:make-value-array (list . ,args)))
	  (result (ffi:empty-alien ,result-type))
	  (result-ptr (ffi:address-of result))
	  (fnspec (if (instance-of? <ffi:cif> ,fnptr)
		      ,fnptr
		      (ffi:make-function-spec
		       (ffi:alien-ffi-type result)
		       (ffi:value-array-types values)))))

     ;; cache the cif
     (unless (instance-of? <ffi:cif> ,fnptr)
       (slot-set! fnspec 'fn-ptr ,fnptr)
       (set! ,fnptr fnspec))

     (ffi:call (slot-ref fnspec 'cif)
	       (slot-ref fnspec 'fn-ptr)
	       (ffi:alien-value result-ptr)
	       (ffi:alien-value values))

    ;; cleanup
    (let ((call-result (ffi:from-alien result)))
      (ffi:free values)
      (ffi:free result)

      call-result)))

(define-syntax (with-library handle-and-name . body)
  "load a dynamic library while body is in scope"
  `(let ((,(first handle-and-name)
	  (ffi:dlopen ,(second handle-and-name))))
     (let ((result (begin . ,body)))
       (ffi:dlclose ,(first handle-and-name))
       result)))

;; simple example of using ffi to resolve symbols
;; already loaded by ld
(with-library (handle nil)
  (let ((lputs (ffi:dlsym handle "puts"))
	(lfork (ffi:dlsym handle "fork"))
	(lgetenv (ffi:dlsym handle "getenv"))
	(lsleep (ffi:dlsym handle "sleep"))
	(lusleep (ffi:dlsym handle "usleep"))
	(lputchar (ffi:dlsym handle "putchar"))
	(lwait (ffi:dlsym handle "wait"))
	(lgetpid (ffi:dlsym handle "getpid"))
	(ltime (ffi:dlsym handle "time"))
	(ltest-fn (ffi:dlsym handle "test_fn")))

    (assert ltest-fn)

    (define (test-fn closure)
      (ffi:funcall ltest-fn 'ffi-void closure))

    (define (getenv var)
      (ffi:alien-to-string
       (ffi:funcall lgetenv 'ffi-pointer var)))

    (define (fork)
      (ffi:funcall lfork 'ffi-uint))

    (define (sleep seconds)
      (ffi:funcall lsleep 'ffi-uint seconds))

    (define (usleep useconds)
      (ffi:funcall lusleep 'ffi-uint useconds))

    (define (getpid)
      (ffi:funcall lgetpid 'ffi-uint))

    (define (time)
      (let* ((mrv (make <ffi:multibyte-raw-value>
		   'length 8))
	     (result (ffi:funcall ltime mrv (ffi:address-of mrv)))
	     (unpacked (ffi:to-bytearray result)))

	(ffi:free-mrv mrv)
	unpacked))


    ;; this definition is a bit trickier because we're
    ;; dealing with a pointer to a primitive
    ;;
    ;; unsigned int wait(unsigned int* status);
    ;;
    (define (wait)
      (let* ((status (ffi:to-alien 0))
	     (pid (ffi:funcall lwait 'ffi-uint (ffi:address-of status))))

	(list (cons 'pid pid)
	      (cons 'status (ffi:from-alien status)))))))

(define (closure-target alien-arg-array)
  (display "I was called! ")
  (display (ffi:alien-to-int
	    (ffi:deref (ffi:get-array-pointer alien-arg-array 0))))
  (newline))

;calls test_fn in the ffi.c file with a function pointer that will
;invoke closure-target when called. Note that create-closure does not
;currently protect the provided function (which can be any arbitrary
;invokeable thing) from gc so it should be protected by other
;means... here closure-target is protected by being a global.
(define (closure-test)
  (let* ((cif (ffi:make-function-spec 'ffi-void (list 'ffi-uint)))
	 (closure (ffi:create-closure (slot-ref cif 'cif)
				      closure-target
				      (ffi:int-to-alien 0))))
    (test-fn closure)))

(define (ffi:fork-test)
  (let ((pid (fork)))
    (if (= pid 0)
	(begin
	  (display "hello from the child")
	  (newline)
	  (sleep 1)
	  (display "child is exiting")
	  (newline)
	  (exit 1))
	(begin
	  (printf "hello from parent. child is %a\n" pid)
	  (display (wait))
	  (newline)))))


