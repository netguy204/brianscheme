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

(display "Building stdlib..." stderr)
(write-char #\newline stdout)

(define (list* . args)
  (letrec ((chase
	    (lambda (args)
	      (cond ((null? args) '())
		    ((null? (cdr args)) (car args))
		    (else (cons (car args) (chase (cdr args))))))))
    (chase args)))

(define (apply* proc . args)
  (apply proc (apply list* args)))

(require 'conditions)
(require 'io)
(require 'math)
(require 'random)
(require 'string)
(require 'point-free)
(require 'clos)
(require 'clos-repl)
(require 'image)
(provide 'stdlib)

(define (repl-or-script)
  "Run either the REPL or the script given in *args*."
  (if (null? *args*)
      (clos-repl)
      (begin
        (load (car *args*))
        (exit 0))))

(repl-or-script)
