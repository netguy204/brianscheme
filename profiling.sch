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

(define (alist-incr! alist key incr)
  "increment key in alist or add it with a value of val if it doesn't
exist"
  (let ((val (assoc key alist)))
    (if val
	(begin
	  (assq-set! alist key (+ (cdr val) incr))
	  alist)
	(cons (cons key incr) alist))))

(define (merge-alists a1 a2)
  (let ((result nil))
    ;; insert things from a1, combining with a2 when possible
    (dolist (item1 a1)
      (let* ((v2p (assoc (car item1) a2))
	     (v2 (if v2p (cdr v2p) 0)))
	(push! (cons (car item1)
		     (+ (cdr item1) v2))
	       result)))

    ;; then insert things from a2 that are unique to that list
    (dolist (item2 a2)
      (let ((v1 (assoc (car item2) a1)))
	(unless v1
	  (push! item2 result))))

    result))

(define (count-calls-out fn)
  (let ((result nil)
	(notepad nil))
    (dolist (instr (%compiled->instructions fn))
	    (cond
	     ((is instr 'fn)
	      ;; recurse into closures
	      (set! result
		    (merge-alists
		     result
		     (count-calls-out (arg1 instr))))
	      ;; note the closure
	      (set! notepad (arg1 instr)))

	     ((is instr 'gvar)
	      ;; if it's been cached, save the name
	      (if (pair? (arg1 instr))
		  (set! notepad (car (arg1 instr)))
		  (set! notepad (arg1 instr))))

	     ((is instr 'incprof)
	      (set! result (alist-incr! result notepad (arg1 instr))))))
    result))

(define-syntax (profile-exp exp)
  `(begin
     (set-profiling! #t)
     ,exp
     (set-profiling! #f)))

