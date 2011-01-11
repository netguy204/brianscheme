; Copyright 2011 Christopher Wellons
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

; DESCRIPTION: Import of my Wisp (lisp) programming language.
; http://git.nullprogram.com/?p=wisp.git

; You'll probably need to define WISPROOT in your environment so that
; it can find the standard libraries.

(require 'ffi)
(require 'clos)

(let* ((wisp (ffi:dlopen "libwisp.so"))
       (init (ffi:dlsym wisp "wisp_init"))
       (repl (ffi:dlsym wisp "repl"))
       (create-str (ffi:dlsym wisp "c_strs"))
       (read-str (ffi:dlsym wisp "lisp_read_string"))
       (c-cons (ffi:dlsym wisp "c_cons"))
       (wisp-nil (ffi:dlsym wisp "NIL"))
       (obj-print (ffi:dlsym wisp "obj_print"))
       (strdup (ffi:dlsym wisp "xstrdup"))
       (c-sym (ffi:dlsym wisp "c_sym"))
       (eval (ffi:dlsym wisp "top_eval")))

  (define (wisp:init)
    "Initialize the Wisp library."
    (ffi:funcall init 'ffi-void))

  (define (wisp:repl)
    "Fire up a Wisp REPL."
    (ffi:funcall repl 'ffi-void))

  (define (wisp:eval-string str)
    "Evaluate the given string in Wisp."
    (let ((f ffi:funcall))
      (f obj-print 'ffi-void
         (f eval 'ffi-pointer
            (f read-str 'ffi-pointer
               (f c-cons 'ffi-pointer
                  (f create-str 'ffi-pointer
                     (f strdup 'ffi-pointer (ffi:string-to-alien str)))
                  (f c-sym 'ffi-pointer (ffi:string-to-alien "nil")))))
         (ffi:int-to-alien 1))
      #t)))

(wisp:init)

(define (wisp:eval sexp)
  "Evaluate an s-expression directly in Wisp."
  (let ((sb (make-string-buffer)))
    (print-object sb sexp)
    (wisp:eval-string (string-buffer->string sb))))
