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
; Tools for parsing out and reporting on the counts the VM is capable
; of keeping when it encounters the incprof instruction
;
; Example usage:
;
; (require 'profiling)
; (prof:profile-exp ( ... do something interesting ... ))
; (prof:profile-state-to-dot "output.dot")
;
; Then you can use the graphviz tools turn this into an image:
; $ dot output.dot -Tpng -ooutput.png
;

(define (prof:alist-incr! alist key incr)
  "increment key in alist or add it with a value of val if it doesn't
exist"
  (let ((val (assoc key alist)))
    (if val
	(begin
	  (assq-set! alist key (+ (cdr val) incr))
	  alist)
	(cons (cons key incr) alist))))

(define (prof:merge-alists a1 a2)
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

(define (prof:list-ref-or-nil lst idx)
  (if (< idx (length lst))
      (list-ref lst idx)
      nil))

(define (prof:count-calls-out fn)
  (let ((result nil)
	(notepad nil))
    (dolist (instr (%compiled->instructions fn))
	    (cond
	     ((is instr 'fn)
	      ;; recurse into closures
	      (set! result
		    (prof:merge-alists
		     result
		     (prof:count-calls-out (arg1 instr))))
	      ;; note the closure
	      (set! notepad (arg1 instr)))

	     ((is instr 'gvar)
	      ;; if it's been cached, save the name
	      (if (pair? (arg1 instr))
		  (set! notepad (car (arg1 instr)))
		  (set! notepad (arg1 instr))))

	     ((is instr 'lvar)
	      ;; try to resolve the lvar if it's baked in
	      (let* ((env (compiled-environment fn))
		     (frame-num (arg1 instr))
		     (slot (arg2 instr))
		     (frame (prof:list-ref-or-nil env frame-num)))
		(if (and (vector? frame)
			 (< slot (vector-length frame))
			 (vector-ref frame slot))
		    (set! notepad (vector-ref frame slot))
		    (set! notepad 'unknown))))

	     ((is instr 'incprof)
	      (set! result (prof:alist-incr! result notepad (arg1 instr))))))
    result))

(define (prof:sym-is-compiled? sym)
  (compiled-procedure? (global-ref sym)))

(define (prof:count-calls-for-sym sym)
  (cons sym (prof:count-calls-out (global-ref sym))))

(define (prof:count-all-calls)
  (let ((compiled-syms (filter prof:sym-is-compiled? (all-symbols))))
    (map prof:count-calls-for-sym compiled-syms)))

(define (prof:merge-all-counts all-counts)
  (reduce
   prof:merge-alists
   (map cdr all-counts)))

(define (prof:resolve-procedures! all-counts)
  (let ((table (make-hashtab-eq 100))
	(idx 0))

    ;; pre-populate table with known symbols
    (dolist (proc-sym (filter prof:sym-is-compiled? (all-symbols)))
      (hashtab-set! table
		    (global-ref proc-sym)
		    proc-sym))

    ;; resolve anonymous closures to distinct ints and named closures
    ;; to their symbol
    (dolist (counts all-counts)
      (dolist (count (cdr counts))
        (when (compiled-procedure? (car count))
	  (begin
	    (unless (hashtab-ref table (car count))
	      ;; it's anonymous. make up a name
	      (hashtab-set! table (car count) idx)
	      (inc! idx))

	    (set-car! count (hashtab-ref table (car count)))))))))

(define (prof:write-dot-file fstream resolved-counts)
  (write-stream fstream "digraph {\n")
  (let ((table (make-hashtab-eq 100))
	(call-totals (make-hashtab-eq 100))
	(idx 0))

    ;; compute the total calls into each node
    (let ((totals (prof:merge-all-counts resolved-counts)))
      (dolist (call totals)
        (hashtab-set! call-totals (car call) (cdr call))))

    ;; resolve the node names
    (dolist (callers resolved-counts)
      (unless (hashtab-ref table (car callers))
        (hashtab-set! table (car callers) idx)
	(inc! idx))

      (dolist (calls (cdr callers))
        (unless (hashtab-ref table (car calls))
          (hashtab-set! table (car calls) idx)
	  (inc! idx))))


    ;; write the node list
    (dolist (node (hashtab-keys table))
      (write-stream
       fstream
       (sprintf "%a [label=\"%a %a\"];\n"
		(hashtab-ref table node)
		node
		(if (hashtab-ref call-totals node)
		    (hashtab-ref call-totals node)
		    -1))))

    ;; write the edge list
    (dolist (callers resolved-counts)
      (dolist (calls (cdr callers))
        (write-stream
	 fstream
	 (sprintf "%a -> %a [headlabel=\"%a\"];\n"
		  (hashtab-ref table (car callers))
		  (hashtab-ref table (car calls))
		  (cdr calls))))))
  (write-stream fstream "}\n"))

(define (prof:made-calls? caller)
  (any?
   (lambda (callee)
     (> (cdr callee) 0))
   (cdr caller)))

(define (prof:get-profile-state)
  (let ((result (prof:count-all-calls)))
    (prof:resolve-procedures! result)
    result))

;; now we're finally to the user interface for this library
(define-syntax (prof:profile-exp exp)
  `(begin
     (set-profiling! #t)
     ,exp
     (set-profiling! #f)))

(define (prof:profile-state-to-dot dotfile)
  (let ((pstate (prof:get-profile-state)))
    (call-with-output-stream dotfile
      (lambda (f)
	(prof:write-dot-file f (filter prof:made-calls? pstate))))))
