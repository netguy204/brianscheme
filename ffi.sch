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
   retspec
   fn-ptr))

(define (ffi:make-function-spec return args fnptr)
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
		  'retspec retspec
		  'fn-ptr fnptr)))

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

(define (ffi:funcall fnptr result-type . args)
  "call alien function expecting result and using default assumed alien types for the given arguments if they're not already alien. This macro may mutate fnptr to cache its function signature"
  (let* ((values (ffi:make-value-array args))
	 (result (ffi:empty-alien result-type))
	 (result-ptr (ffi:address-of result))
	 (fnspec (ffi:make-function-spec result-type (map ffi:alien-type args) fnptr)))

     (ffi:call (ffi:cif-cif-ref fnspec) (ffi:cif-fn-ptr-ref fnspec) result-ptr (ffi:values-array-ref values))

    ;; cleanup
    (let ((call-result (ffi:from-alien result result-type)))
      call-result)))

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

(define (ffi:gen-get-const include const)
  "generate c code to print out a constant numeric value"
  (ffi:include-and-main include
    (sprintf
"
  printf(\"%d\\n\", %s);
" const)))

(define (ffi:get-const include const)
  (compile-and-run (ffi:gen-get-const include const)))

(define (ffi:offset-of include type field)
  "generate c code to print the offset of FIELD in TYPE"
  (ffi:get-const include (sprintf "(int)&(((%s *)0)->%s)" type field)))

(define (ffi:size-of include type)
  "generate c code to print the size of TYPE"
  (ffi:get-const include (sprintf "sizeof(%s)" type)))

(define (ffi:size-of-long)
  "returns machine size of long. caches result"
  (let-static ((sz ()))
    (unless sz
      (set! sz (ffi:size-of "<stdlib.h>" "long")))
    sz))

(define (ffi:endianess)
  "returns big-endian or little-endian"
  (let-static ((result ()))
    (unless result
      (let ((big (ffi:get-const "<endian.h>" "__BIG_ENDIAN"))
	    (little (ffi:get-const "<endian.h>" "__LITTLE_ENDIAN"))
	    (ours (ffi:get-const "<endian.h>" "__BYTE_ORDER")))
	(set! result
	      (cond
	       ((= ours big) 'big-endian)
	       ((= ours little) 'little-endian)
	       (else (throw-error "unrecognized byte order" ours))))))
    
    result))

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

(define (ffi:integer->long-bytes int)
  "create a list of N bytes with the most significant first"
  (let ((num-bytes (ffi:size-of-long)))
    (let loop ((count 0)
	       (value int)
	       (result nil))
      (if (< count num-bytes)
	  (loop (+ count 1)
		(ash value -8)
		(cons (logand value #Xff) result))
	  result))))

(define (ffi:long-bytes->integer bytes)
  "convert a most-significant-first array of bytes into an integer"
  (assert (<= (length bytes) (ffi:size-of-long)))
  (let loop ((remaining bytes)
	     (result 0))
    (if remaining
	(loop (rest remaining)
	      (logor (ash result 8) (first remaining)))
	result)))

(define (ffi:pack-bytes bytes offset to-pack)
  "pack TO-PACK bytes into BYTES starting at OFFSET"
  (dolist-idx ((val idx) to-pack)
    (ffi:byte-set! bytes (+ offset idx) (if (char? val)
					    val
					    (integer->char val)))))

(define (ffi:unpack-bytes bytes offset count)
  "unpack COUNT bytes from BYTES starting at OFFSET"
  (let ((result nil))
    (dotimes (idx count)
      (push! (char->integer (ffi:byte-ref bytes (+ offset idx))) result))
    (reverse result)))

(define (ffi:pack-long bytes offset long)
  "pack machine sized LONG into BYTES starting at OFFSET"
  (ffi:pack-bytes bytes 0 (ffi:bs->machine (ffi:integer->long-bytes long)))
  bytes)

(define (ffi:unpack-long bytes offset)
  "unpack machine sized long from BYTES starting at OFFSET"
  (ffi:long-bytes->integer (ffi:machine->bs (ffi:unpack-bytes bytes offset (ffi:size-of-long)))))

(define (ffi:create-long-example long)
  (ffi:pack-long (ffi:make-bytes (ffi:size-of-long)) 0 long))

(define (ffi:gcc-test)
  "examples of using the ffi code that depends on gcc"
  (let ((include "\"types.h\""))
    (printf "object is %a bytes\n" (ffi:size-of include "object"))
    (printf "string is at offset %a\n" (ffi:offset-of include "object" "data.string.value"))))

