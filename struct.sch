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

; DESCRIPTION: Thin wrapper over a vector that makes it feel more like
; a struct


(define-syntax (define-struct name . docs-and-slots)
  "create a structure of a given name with a set of slots"
  (let* ((builder-sym (string->symbol
		       (string-append "make-"
				      (symbol->string name))))
	 (tester-sym (string->symbol
		      (string-append (symbol->string name) "?")))

	 (slots (if (string? (car docs-and-slots))
		    (car (cdr docs-and-slots))
		    (car docs-and-slots)))
	 (num-slots (length slots))
	 (slot-numbers nil)
	 (args (gensym))
	 (struct (gensym))
	 (value (gensym))
	 (slot-defaults nil))

    ;; add a documentation entry if we got a docstring
    (when (string? (car docs-and-slots))
	  (add-documentation name (car docs-and-slots)))

    ;; collect data to build the slot accessors
    (let ((idx 1))
      (dolist (slot slots)
	;; vector idx to find the slot
        (push! (cons (car slot) idx)
	       slot-numbers)

	;; default value if none given in constructor
	(push! (cons (car slot)
		     (if (null? (cdr slot))
			 nil
			 (second slot)))
	       slot-defaults)
	(inc! idx)))

    (letrec ((get-slot-namer (lambda (slot-name)
			       (string->symbol
				(string-append (symbol->string name)
					"-"
					(symbol->string slot-name)))))
	     (set-slot-namer (lambda (slot-name)
			       (string->symbol
				(string-append "set-"
					(symbol->string name)
					"-"
					(symbol->string slot-name)
					"!")))))

      ;; define the getters, setters, and constructor
      `(begin
	 ;; constructor
	 (define (,builder-sym . ,args)
	   ,(string-append "create a structure of type "
		    (symbol->string name))
	   (let ((struct (make-vector ,(+ 1 num-slots) nil)))
	     (vector-set! struct 0 ',name)

	     (dolist (slot ',slots)
	       (let ((val (member (car slot) ,args)))
		 (if val
		     (vector-set! struct
				  (cdr (assoc (car slot)
					      ',slot-numbers))
				  (second val))
		     (vector-set! struct
				  (cdr (assoc (car slot)
					      ',slot-numbers))
				  (cdr (assoc (car slot)
					      ',slot-defaults))))))

	     struct))

	 (define (,tester-sym struct)
	   ,(string-append "test to see if structure is of type "
			   (symbol->string name))
	   (and (vector? struct) (eq? (vector-ref struct 0) ',name)))

	 ;; getters
	 ,(map (lambda (slot)
		 `(define (,(get-slot-namer (car slot)) struct)
		    ,(string-append "retrieve slot "
			     (symbol->string (car slot))
			     " of "
			     (symbol->string name))
		    (vector-ref struct ,(cdr (assoc (car slot)
						    slot-numbers)))))
	       slots)

	 ;; setters
	 ,(map (lambda (slot)
		 `(define (,(set-slot-namer (car slot)) struct value)
		    ,(string-append "set slot "
			     (symbol->string (car slot))
			     " of "
			     (symbol->string name))
		    (vector-set! struct
				 ,(cdr (assoc (car slot)
					      slot-numbers))
				 value)))
	       slots)))))


