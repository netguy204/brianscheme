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
  (letrec ((chase
	    (lambda (args)
	      (cond ((null? args) '())
		    ((null? (cdr args)) (car args))
		    (else (cons (car args) (chase (cdr args))))))))
    (chase args)))

(define (plist-get list key)
  "Return property value in plist."
  (second (member key list)))

(define (plist-set! list key value)
  "Set property value in plist."
  (let ((rest (member key list)))
    (set-cdr! rest (cons value (cddr rest)))))

(define (apply* proc . args)
  (apply proc (apply list* args)))

(define (map-vector fn vector)
  (let* ((len (vector-length vector))
	 (result (make-vector len nil)))
    (dotimes (idx len)
      (vector-set! result idx
		   (fn (vector-ref vector idx))))
    result))

(define (vector->list vector)
  (let ((result nil)
	(len (vector-length vector)))
    (dotimes (idx len)
      (push! (vector-ref vector idx) result))
    (reverse result)))

(require 'cl-defun)
(require 'conditions)
(require 'io)
(require 'math)
(require 'string)
(require 'point-free)
(require 'clos)
(require 'clos-repl)
(require 'image)
(require 'random)
(provide 'stdlib)

(define (repl-or-script)
  "Run either the REPL or the script given in *args*."
  (if (null? *args*)
      (clos-repl)
      (begin
        (load (car *args*))
        (exit 0))))

(repl-or-script)
