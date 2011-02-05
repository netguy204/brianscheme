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
; A simple exception/condition system

;; this is the dynamic binding that holds the information for
;; resolving the current handler of signals

;;
;; the base case is a hard exit, the repl will install something more
;; friendly
;;

(let ((condition-handlers nil))
  (define (conditions:push-handler handler)
    (push! handler condition-handlers))

  (define (conditions:top-handler)
    (car condition-handlers))

  (define (conditions:handlers?)
    (not (null? condition-handlers)))

  (define (conditions:pop-handler)
    (pop! condition-handlers)))


(define (raise obj)
  "throw object up to the currently installed *condition-handler*"
  (if (conditions:handlers?)
      ((conditions:top-handler) obj)
      (begin
	(write-port "unhandled condition: " stderr)
	(write-port obj stderr)
	(newline)
	(exit 1))))

(define-syntax (with-nonlocal-exit exiter flag . body)
  (let ((cc (gensym)))
    `(call/cc (lambda (,cc)
		(let ((,exiter
		       (lambda x (,cc (cons ,flag x)))))
		  . ,body)))))

(define-syntax (guard exception-and-clauses . body)
  (let ((ex (gensym))
	(flag (gensym))
	(result (gensym))
	(exception (car exception-and-clauses))
	(clauses (cdr exception-and-clauses)))

    `(let ((,result
	    (with-nonlocal-exit ,ex ',flag
	      (conditions:push-handler ,ex)
	      . ,body)))

       (conditions:pop-handler)

       (if (and (pair? ,result)
		(eq? (car ,result) ',flag))
	   ;; an exception was thrown
	   (begin
	     (let ((,exception (cdr ,result)))
	       (cond
		,@clauses
		(#t (raise ,exception)))))

	   ;; exited normally
	   ,result))))


;; replace error and throw error with exception raising equivalents
(define (error . objs)
  "raise an error-condition"
  (raise (cons 'error-condition objs)))

(define throw-error error)

