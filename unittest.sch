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

; DESCRIPTION: Provides functions useful for defining and executing
; unit tests
;

(defvar *test-name* nil)

(define (report-result result form)
  "print out form annotated with the truth value of result"
  (let ((result-str (if result "pass" "FAIL"))
	(name-str (if (*test-name*) (concat 
				     (symbol->string (*test-name*))
				     ":")
		      "")))
    (printf "%s ... %s %a\n" result-str name-str form)
    result))

(define-syntax (combine-results . forms)
  "like AND but not short circuiting"
  (let ((result (gensym)))
    `(let ((,result #t))
       (begin . ,(map (lambda (form)
			`(unless ,form (set! ,result #f)))
		      forms))
       ,result)))


(define (copy-tree tree)
  "create a deep of tree so macroexpansion doesn't gobble it up"
  (cond
   ((pair? tree) (cons (copy-tree (car tree))
		       (copy-tree (cdr tree))))
   (else tree)))
		   

(define-syntax (check . forms)
  "run report result for each form in forms"
  `(combine-results . ,(map (lambda (form)
			      (let ((copy (gensym)))
				`(let ((,copy (copy-tree ',form)))
				   (report-result ,form ,copy))))
			    forms)))

(define-syntax (define-test name-and-params . body)
  `(define ,name-and-params
     (binding ((*test-name* ',(first name-and-params)))
       . ,body)))


(define-test (foo)
  (check (= (+ 1 2) 3)
	 (< 1 2)))
