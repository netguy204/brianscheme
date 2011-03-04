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

; DESCRIPTION:
;
; At this point boot.sch has executed and recompiled itself and we're
; now in a fully bootstrapped and compile only environment. Now it's
; appropriate to start building up the rest of the language niceties
; that weren't essential for bootstrapping.

;; define a brutal restart handler. clos-repl or a user script
;; candefine a more friendly one later
(set-error-restart!
 (lambda (ex)
   (display "VM got exception: " stderr)
   (display ex stderr)
   (write-char #\newline stderr)
   (exit 1)))

(display "Building stdlib..." stderr)
(write-char #\newline stderr)

(define (list* . args)
  "appends any number of elements to the list that is the last
argument"
  (letrec ((chase
	    (lambda (args)
	      (cond ((null? args) '())
		    ((null? (cdr args)) (car args))
		    (else (cons (car args) (chase (cdr args))))))))
    (chase args)))

(define (apply* proc . args)
  "apply a function to a series of arguments (the last of which is a
list)"
  (apply proc (apply list* args)))

(define (map-vector fn vector)
  "apply a function to each element of a vector and return a vector of
the results"
  (let* ((len (vector-length vector))
	 (result (make-vector len nil)))
    (dotimes (idx len)
      (vector-set! result idx
		   (fn (vector-ref vector idx))))
    result))

(define (vector->list vector)
  "convert a vector of elements into a list of elements in the same
order"
  (let ((result nil)
	(len (vector-length vector)))
    (dotimes (idx len)
      (push! (vector-ref vector idx) result))
    (reverse result)))

(define (system cmd)
  "Run command using the system's shell."
  (assert-types (cmd string?))
  (%system cmd))

(define (getenv var)
  "Get an environmental variable."
  (assert-types (var string?))
  (%getenv var))

(define (interned? sym)
  "Return #t if symbol is interned."
  (eq? sym (string->symbol (symbol->string sym))))

(define (chdir path)
  "Change the current working directory."
  (assert-types (path string?))
  (%chdir path))

(define (date-string)
  "Return current date as a standard string."
  (let ((str (%date-string)))
    (substring str 0 (- (string-length str) 1))))

(require 'point-free)
(require 'string)
(require 'io)
(require 'conditions)
(require 'math)
(require 'clos)
(require 'read)
(require 'sugar)
(require 'clos-repl)
(require 'list)
(require 'image)
(require 'random)

(define-syntax (doto exp . funcs)
  "As Clojure's doto, apply function forms to exp as left-most argument."
  (let ((res (gensym)))
    `(let ((,res ,exp))
       ,@(map [cons (car _) (cons res (cdr _))] funcs)
       ,res)))

(provide 'stdlib)

(define (repl-or-script)
  "Run either the REPL or the script given in *args*."
  (if (null? *args*)
      (clos-repl)
      (begin
        (load (car *args*))
        (exit 0))))

(repl-or-script)
