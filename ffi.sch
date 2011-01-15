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

(define-struct ffi:cif
  "internal representation of cif. holds onto stuff that needs to be
freed later."
  ((cif)
   (argspec)
   (retspec)
   (fn-ptr)))

(define (ffi:free-cif cif)
  (assert (ffi:cif? cif))
  (ffi:free (ffi:cif-cif cif))
  (ffi:free (ffi:cif-argspec cif))
  (ffi:free (ffi:cif-retspec cif)))

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

    (make-ffi:cif 'cif cif
		  'argspec argspec
		  'retspec retspec)))

(define-struct ffi:alien-type
  "a wrapped alien type"
  ((type)
   (value)))

(define (ffi:alien-string str)
  "create an alien string"
  (make-ffi:alien-type
   'type 'ffi-pointer
   'value (ffi:string-to-alien str)))

(define (ffi:alien-uchar val)
  "create an alien unsigned char"
  (make-ffi:alien-type
   'type 'ffi-uchar
   'value (ffi:int-to-alien val)))

(define (ffi:alien-ushort val)
  "create an alien unsigned short"
  (make-ffi:alien-type
   'type 'ffi-ushort
   'value (ffi:int-to-alien val)))

(define (ffi:alien-uint val)
  "create an alien unsigned int"
  (make-ffi:alien-type
   'type 'ffi-uint
   'value (ffi:int-to-alien val)))

(define (ffi:to-alien obj)
  "convert obj to its corresponding alien representation"
  (cond
   ((string? obj) (ffi:string-to-alien obj))
   ((integer? obj) (ffi:int-to-alien obj))
   ((ffi:alien-type? obj) (ffi:alien-type-value obj))
   ((alien? obj) obj)
   (else (throw-error "can't convert" obj "to alien"))))

(define (ffi:from-alien obj type)
  "convert obj from alien assuming that it's of the given type"
  (case type
    (ffi-uint (ffi:alien-to-int obj))
    (ffi-pointer obj)
    (ffi-void nil)
    (else (throw-error "can't convert" type "back from alien"))))

(define (ffi:alien-type obj)
  "determine the alien type tag for a given object"
  (cond
   ((string? obj) 'ffi-pointer)
   ((alien? obj) 'ffi-pointer)
   ((integer? obj) 'ffi-uint)
   ((ffi:alien-type? obj) (ffi:alien-type-type obj))
   (else (throw-error "don't know ffi type for" obj))))

(define (ffi:empty-alien type)
  "allocate default initialized alien value of a given type"
  (case type
    (ffi-uint (ffi:int-to-alien 0))
    (ffi-pointer (ffi:address-of (ffi:int-to-alien 0)))
    (ffi-void nil)
    (else (throw-error "can't make empty" type))))

(define-struct ffi:values
  "an array of alien values as void**"
  ((array)
   (list)
   (ptr-list)))

(define (ffi:free-values values)
  (assert (ffi:values? values))
  (ffi:free (ffi:values-array values))
  (for-each ffi:free (ffi:values-list values))
  (for-each ffi:free (ffi:values-ptr-list values)))

(define (ffi:make-value-array args)
  "convert a list of scheme and alien arguments into a void**"
  (let* ((values (ffi:make-pointer-array (length args)))
	 (value-list (map ffi:to-alien args))
	 (value-ptr-list (map ffi:address-of value-list)))

    (dolist-idx ((val idx) value-ptr-list)
      (ffi:set-array-pointer! values idx val))

    (make-ffi:values 'array values
		     'list value-list
		     'ptr-list value-ptr-list)))

(define-syntax (ffi:funcall fnptr result-type . args)
  "call alien function expecting result and using default assumed alien types for the given arguments if they're not already alien. This macro may mutate fnptr to cache its function signature"
  `(let* ((values (ffi:make-value-array (list . ,args)))
	  (result (ffi:empty-alien ,result-type))
	  (result-ptr (ffi:address-of result))
	  (fnspec (if (ffi:cif? ,fnptr)
		      ,fnptr
		      (ffi:make-function-spec
		       ,result-type
		       (map ffi:alien-type (list . ,args))))))

     ;; cache the cif
     (unless (ffi:cif? ,fnptr)
       (set-ffi:cif-fn-ptr! fnspec ,fnptr)
       (set! ,fnptr fnspec))

     (ffi:call (ffi:cif-cif fnspec) (ffi:cif-fn-ptr fnspec) result-ptr (ffi:values-array values))

    ;; cleanup
    (let ((call-result (ffi:from-alien result ,result-type)))
      ;(ffi:free-values values)
      ;(ffi:free-cif fnspec)
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
	(ltest-fn (ffi:dlsym handle "test_fn")))

    (assert ltest-fn)

    (define (test-fn closure)
      (ffi:funcall ltest-fn 'ffi-void closure))

    (define (getenv var)
      (let ((result
	     (ffi:funcall lgetenv 'ffi-pointer
			  (ffi:string-to-alien var))))
	(if (= (ffi:alien-to-int result) 0)
	    #f
	    (ffi:alien-to-string result))))

    (define (fork)
      (ffi:funcall lfork 'ffi-uint))

    (define (sleep seconds)
      (ffi:funcall lsleep 'ffi-uint seconds))

    (define (usleep useconds)
      (ffi:funcall lusleep 'ffi-uint useconds))


    ;; this definition is a bit trickier because we're
    ;; dealing with a pointer to a primitive
    ;;
    ;; unsigned int wait(unsigned int* status);
    ;;
    (define (wait)
      (let* ((status (ffi:int-to-alien 0))
	     (pid (ffi:funcall lwait 'ffi-uint (ffi:address-of status))))

	(list (cons 'pid pid)
	      (cons 'status (ffi:alien-to-int status)))))))

(define (closure-target alien-arg-array)
  (display "I was called! ")
  (display (ffi:alien-to-int
	    (ffi:deref (ffi:get-array-pointer alien-arg-array 0))))
  (newline))

;calls test_fn in the ffi.c file with a function pointer that will
;invoke closure-target when called. Note that create-closure does not
;currently protect the provided function (which can be any arbitrary
;invokeable thing) from gc so it should be protected by other
;means... here closure-target protected by being a global.
(define (closure-test)
  (let* ((cif (ffi:make-function-spec 'ffi-void (list 'ffi-uint)))
	 (closure (ffi:create-closure (ffi:cif-cif cif)
				      closure-target
				      (ffi:alien-to-int 0))))
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
	  (display "hello from parent. child is ")
	  (display pid)
	  (newline)
	  (display (wait))
	  (newline)))))


