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
; Provides CL-style keyword arguments

(define (parse-args arglist)
  (let ((bindings nil)
	(argnames nil)
	(boa? #t)
	(rest-sym (gensym)))

    (let loop ((rem arglist))
      (cond
       ((null? rem) #t) ; done
       ((symbol? rem)
	;; we have a dotted arglist
	(unless boa?
          (throw-error "got" arglist
		       "dotted arglist only supported if the entire arglist is BOA"))

	(set! boa? #f)
	(set! rest-sym rem))

       ((pair? rem)
	;; deal with standard arguments
	(let ((arg (car rem)))
	  (cond
	   ((symbol? arg)
	    (unless boa?
		    (throw-error "argument list contained a BOA arg" arg
				 "that followed a positionless arg"))
	    (push! arg argnames))

	   ((pair? arg)
	    (let (((varname default) arg)
		  (sym (gensym)))

	      (set! boa? #f)
	      (push! `(,varname (getl ,rest-sym ',varname ,default))
		     bindings)))

	   (else
	    (throw-error arg "is not valid in an argument list"))))
	;; go to the next arg
	(loop (cdr rem)))

       (else
	(throw-error "bad argument" rem
		     "the dotted tail of an arglist must be a symbol"))))



    (if (not boa?)
	;; we need rest-args
	(set! argnames `(,@(reverse argnames) . ,rest-sym))
	;; no need for rest-args
	(set! argnames (reverse argnames)))

    (list argnames (reverse bindings))))


(define-syntax (cl-lambda args . body)
  (let ((argnames-and-bindings (parse-args args)))
    `(lambda ,(first argnames-and-bindings)
       ,(if (second argnames-and-bindings)
	    `(let ,(second argnames-and-bindings)
	      . ,body)
	    `(begin . ,body)))))

(define-syntax (define name . value-or-body)
  (cond
   ((symbol? name)
    (cond
     ((length=1 value-or-body)
      ;; this is defining a simple value
      `(set! ,name ,(first value-or-body)))
     ((and (= (length value-or-body) 2)
	   (string? (second value-or-body)))
      ;; this is a simple value with a doc-string
      (add-documentation name (second value-or-body))
      `(set! ,name ,(first value-or-body)))))

   ((pair? name)
    ;; we must be defining a function
    (unless (symbol? (first name))
      (throw-error "got" (first name) "but name must be a symbol"))

    (let ((name (first name))
	  (arglist (cdr name))
	  (doc (if (string? (first value-or-body))
		   (first value-or-body)
		   ""))
	  (body (if (string? (first value-or-body))
		    (cdr value-or-body)
		    value-or-body)))

      (add-documentation name doc)
      `(set! ,name (cl-lambda ,arglist ,@body))))

   (else
    (throw-error "define: don't know how to handle" name))))

