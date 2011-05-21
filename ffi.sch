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
  "internal representation of cif. holds on to stuff that needs to be
garbage collected no earlier than the cif"
  (cif
   argspec
   retspec))

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
  (type
   value))

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
   ((ffi:alien-type? obj) (ffi:alien-type-value-ref obj))
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
   ((ffi:alien-type? obj) (ffi:alien-type-type-ref obj))
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
  (array
   list
   ptr-list))

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
  "call alien function expecting result and using default assumed
alien types for the given arguments if they're not already alien. This
macro may mutate fnptr to cache its function signature"
  `(let-static ((fnspec ()))
     (let* ((values (ffi:make-value-array (list . ,args)))
	    (result (ffi:empty-alien ,result-type))
	    (result-ptr (ffi:address-of result)))

       (unless fnspec
         (set! fnspec (ffi:make-function-spec
		       ,result-type
		       (map ffi:alien-type (list . ,args)))))

       (ffi:call (ffi:cif-cif-ref fnspec) ,fnptr result-ptr (ffi:values-array-ref values))

     (let ((call-result (ffi:from-alien result ,result-type)))
       call-result))))

(define-syntax (with-library handle-and-name . body)
  "provide a handle to a dynamic library while body is in scope. The
body will be re-executed with a new handle whenever the image is
reloaded."
  `(let ((loader
	  (lambda ()
	    (let ((,(first handle-and-name)
		   (ffi:dlopen ,(second handle-and-name))))
	      (let ((result (begin . ,body)))
		result)))))

     (push! loader *load-hooks*)
     (loader)))

;; simple example of using ffi to resolve symbols
;; already loaded by ld
(with-library (handle nil)
  (let ((lputs (ffi:dlsym handle "puts"))
	(lfork (ffi:dlsym handle "fork"))
	(lsleep (ffi:dlsym handle "sleep"))
	(lusleep (ffi:dlsym handle "usleep"))
	(lputchar (ffi:dlsym handle "putchar"))
	(lwait (ffi:dlsym handle "wait"))
	(ltime (ffi:dlsym handle "time"))
	(ltest-fn (ffi:dlsym handle "test_fn")))

    (assert ltest-fn)

    (define (test-fn closure)
      (ffi:funcall ltest-fn 'ffi-void closure))

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

;;calls test_fn in the ffi.c file with a function pointer that will
;;invoke closure-target when called. Note that create-closure does not
;;currently protect the provided function (which can be any arbitrary
;;invokeable thing) from gc so it should be protected by other
;;means... here closure-target protected by being a global.
(define (closure-test)
  (let* ((cif (ffi:make-function-spec 'ffi-void (list 'ffi-uint) nil))
	 (closure (ffi:create-closure (ffi:cif-cif-ref cif)
				      closure-target)))
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


(define (compile-and-run code)
  "use gcc to compile code and then execute it using the reader to
capture whatever it writes to stdout"
  (let ((tempfile "temp.c")
	(tempcomp "temp"))
    (with-output-stream (stream tempfile)
      (write-stream stream code))

    (let ((cmd (sprintf "gcc -o %s %s" tempcomp tempfile)))
      (unless (system cmd)
        (throw-error "command failed" cmd)))

    (let* ((pipe (open-input-pipe (sprintf "./%s" tempcomp)))
	   (result (read-port pipe)))
      (close-input-port pipe)
      (system (sprintf "rm -f %s %s" tempfile tempcomp))
      result)))

(define (ffi:include-and-main include main)
  (sprintf
"
#include <stdio.h>
#include %s

int main(int argc, char ** argv) {
%s
}
" include main))

(define (ffi:gen-printf format const)
  (sprintf
"
  printf(\"%s\\n\", %s);
" format const))

(define (ffi:gen-get-const include format const)
  "generate c code to print out a constant numeric value"
  (ffi:include-and-main
   include
   (ffi:gen-printf format const)))

(define (ffi:gen-get-consts include format-consts)
  (ffi:include-and-main
   include
   (apply string-append
	  `(,(ffi:gen-printf "(" "0")
	    ,@(map [ffi:gen-printf (first _) (second _)]
		   format-consts)
	    ,(ffi:gen-printf ")" "0")))))

(define (ffi:get-const include format const)
  (compile-and-run (ffi:gen-get-const include format const)))

(define (ffi:get-consts include format-and-consts)
  (compile-and-run (ffi:gen-get-consts include format-and-consts)))

(define (ffi:offsets-of include type fields)
  "generate c code to print the offsets of FIELDS in TYPE"
  (ffi:get-consts include
    (map (lambda (field)
	   (list "%ld" (sprintf "(long)&(((%s *)0)->%s)" type field)))
	 fields)))

(define (ffi:offset-of include type field)
  "generate c code to print the offset of FIELD in TYPE"
  (first (ffi:offsets-of include type (list field))))

(define (ffi:sizes-of include types)
  "generate c code to print the size of TYPE"
  (ffi:get-consts include
    (map (lambda (type)
	   (list  "%ld" (sprintf "sizeof(%s)" type)))
	 types)))

(define (ffi:size-of include type)
  "generate c code to print the size of TYPE"
  (first (ffi:sizes-of include (list type))))

(define-constant-function ffi:size-of-long
  (ffi:size-of "<stdlib.h>" "long"))

(define-constant-function ffi:endianess
  (let* ((fields (ffi:get-consts "<endian.h>" '(("%d" "__BIG_ENDIAN")
						("%d" "__LITTLE_ENDIAN")
						("%d" "__BYTE_ORDER"))))
	 (big (first fields))
	 (little (second fields))
	 (ours (third fields)))
    (cond
     ((= ours big) 'big-endian)
     ((= ours little) 'little-endian)
     (else (throw-error "unrecognized byte order" ours)))))

(define (ffi:little-endian?)
  (eq? (ffi:endianess) 'little-endian))

(define (ffi:bs->machine bytes)
  "put BYTES into machine order from bs order"
  (if (ffi:little-endian?)
      (reverse bytes)
      bytes))

(define (ffi:machine->bs bytes)
  "put BYTES into bs order from machine order"
  (if (ffi:little-endian?)
      (reverse bytes)
      bytes))

(define (ffi:integer->bytes int num-bytes)
  "convert INT into NUM-BYTES bytes with most significant first"
  (let loop ((count 0)
	     (value int)
	     (result nil))
    (if (< count num-bytes)
	(loop (+ count 1)
	      (ash value -8)
	      (cons (logand value #Xff) result))
	result)))

(define (ffi:integer->long-bytes int)
  "create a list of N bytes with the most significant first"
  (let ((num-bytes (ffi:size-of-long)))
    (ffi:integer->bytes int num-bytes)))

(define (ffi:long-bytes->integer bytes)
  "convert a most-significant-first array of bytes into an integer"
  (assert (<= (length bytes) (ffi:size-of-long)))
  (let loop ((remaining bytes)
	     (result 0))
    (if remaining
	(loop (rest remaining)
	      (logor (ash result 8) (first remaining)))
	result)))

(define (ffi:set-bytes bytes offset count value)
  "set COUNT bytes to VALUE in BYTES starting at OFFSET"
  (dotimes (idx count)
    (ffi:byte-set! bytes (+ offset idx) (if (char? value)
					    value
					    (integer->char value))))
  bytes)

(define (ffi:pack-bytes bytes offset to-pack)
  "pack TO-PACK bytes into BYTES starting at OFFSET"
  (dolist-idx ((val idx) to-pack)
    (ffi:byte-set! bytes (+ offset idx) (if (char? val)
					    val
					    (integer->char val))))
  bytes)

(define (ffi:unpack-bytes bytes offset count)
  "unpack COUNT bytes from BYTES starting at OFFSET"
  (let ((result nil))
    (dotimes (idx count)
      (push! (char->integer (ffi:byte-ref bytes (+ offset idx))) result))
    (reverse result)))

(define (ffi:pack-long bytes offset long)
  "pack machine sized LONG into BYTES starting at OFFSET"
  (ffi:pack-bytes bytes offset (ffi:bs->machine (ffi:integer->long-bytes long)))
  bytes)

(define (ffi:unpack-integer bytes offset num-bytes)
  "unpack an integer of NUM-BYTES from BYTES at OFFSET"
  (ffi:long-bytes->integer (ffi:machine->bs (ffi:unpack-bytes bytes offset num-bytes))))

(define (ffi:make-int-unpacker sz)
  "make an int unpacker that unpacks ints of size SZ"
  (lambda (bytes offset)
    (ffi:unpack-integer bytes offset sz)))

(define (ffi:unpack-long bytes offset)
  "unpack machine sized long from BYTES starting at OFFSET"
  (ffi:unpack-integer bytes offset (ffi:size-of-long)))

(define (ffi:pack-byte bytes offset byte)
  (ffi:pack-bytes bytes offset (list (if (char? byte)
					 (char->integer byte)
					 byte))))

(define (ffi:unpack-byte bytes offset)
  (first (ffi:unpack-bytes bytes offset 1)))

(define (ffi:unpack-pointer bytes offset)
  (ffi:deref (ffi:offset-pointer bytes offset)))

(define (ffi:symbol-stringify sym-or-string)
  "convert SYM-OR-STRING into a string if it's a symbol"
  (if (symbol? sym-or-string)
      (symbol->string sym-or-string)
      sym-or-string))

(define (ffi:symbol-append sym-or-string value)
  "create a new value that is SYM-OR-STRING concatenated with VALUE"
  (string->symbol (prim-concat (ffi:symbol-stringify sym-or-string)
			       (ffi:symbol-stringify value))))

(define (ffi:compute-enums include names-and-symbols)
  "compute the numeric value of each of NAMES-AND-SYMBOLS by parsing
INCLUDE"
  (let ((enums (ffi:get-consts include
			       (map (lambda (nas)
				      (list "%d" (ffi:symbol-stringify (first nas))))
				    names-and-symbols))))

    (map cons enums names-and-symbols)))

(define-syntax (ffi:define-enum include cname lisp-name . names-and-symbols)
  "create a pack and unpack method for enumerated values of CNAME
under names that are suffixed with LISP-NAME"
  ;; fixme: INCLUDE has to literally be a string... how can i get it
  ;; to evaluate to whatever what the user passes is bound to?
  (let* ((ens (ffi:compute-enums include names-and-symbols))
	 (sz (ffi:size-of include (ffi:symbol-stringify cname)))
	 (sym-to-num (map (lambda (e)
			    (list (third e) (first e)))
			  ens))
	 (num-to-sym (map (lambda (e)
			    (list (first e) (third e)))
			  ens)))
    `(begin
       (define (,(ffi:symbol-append "pack-" lisp-name)
		bytes offset symbol)
	 "pack SYMBOL into BYTES at OFFSET"
	 (let ((e (assoc symbol ',sym-to-num)))
	   (unless e
	     (throw-error "symbol" symbol "is not a valid enum value"))
	   (ffi:pack-bytes bytes offset
			   (ffi:bs->machine
			    (ffi:integer->bytes (second e) ,sz)))
	   bytes))

       (define (,(ffi:symbol-append "unpack-" lisp-name)
		bytes offset)
	 "unpack a symbol from BYTES at OFFSET"
	 (let* ((val (ffi:unpack-integer bytes offset 1))
		(e (assoc val ',num-to-sym)))
	   (unless e
	     (throw-error val "is not a valid enumerated value"))
	   (second e)))

       (define (,(ffi:symbol-append "size-of-" lisp-name))
	 ,sz))))

(define-syntax (ffi:define-header-struct header type lisp-name . fields)
  ;; format of fields is ("field-name" lisp-field-name field-parser)
  (let* ((offsets (ffi:offsets-of header type (map first fields)))
	 (field-handlers (map (lambda (off field)
				(let ((fn (eval (third field))))
				  (lambda (bytes offset)
				    ;; emit an alist entry
				    (list (second field)
					  (fn bytes (+ offset off))))))
			      offsets fields)))

    `(define (,lisp-name bytes offset)
       (map (lambda (handler)
	      (handler bytes offset))
	    ',field-handlers))))

(define (ffi:create-long-example long)
  (ffi:pack-long (ffi:make-bytes (ffi:size-of-long)) 0 long))

(define (ffi:gcc-test)
  "examples of using the ffi code that depends on gcc"
  (let ((include "\"types.h\""))
    (printf "object is %a bytes\n" (ffi:size-of include "object"))
    (printf "string is at offset %a\n" (ffi:offset-of include "object" "data.string.value"))))

