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

    (dolist (arg arglist)
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

    (if (not boa?)
	;; we need rest-args
	(set! argnames `(,@(reverse argnames) . ,rest-sym))
	;; no need for rest-args
	(set! argnames (reverse argnames)))

    (list argnames (reverse bindings))))


(define-syntax (cl-lambda args . body)
  (let ((argnames-and-bindings (parse-args args)))
    `(lambda ,(first argnames-and-bindings)
       (let ,(second argnames-and-bindings)
	 . ,body))))
