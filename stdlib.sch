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
; In this environment we don't have a working define or define-syntax
; and all of the primitives do no type checking. We'll be building up
; a more friendly environment from here.
;
; I'm using the fname0 notation to denote functions that are nearly
; primitive and not intended for everyday use. None of these '0'
; functions do type checking and often they are just renamed versions
; of their original primitive self so that we can replace their name
; with something more user-friendly.
;
; I haven't made much of an attempt to stick with the conventions
; defined in the scheme R5/6 specs. Some of the names are similar.
; Some of the methods are similar. Maybe I'll get around to going
; back and doing things more by the book. Most of these methods were
; simply written as they were required.


;; Some very basic defines to get us started

(set! nil '())

(set! define-syntax
      (macro (name-and-vars . body)
	`(set! ,(car name-and-vars)
	       (macro ,(cdr name-and-vars)
		 (begin . ,body)))))

(define-syntax (define name . value-or-body)
  (if (symbol? name)
      (if (null? value-or-body)
	  `(set! ,name nil)
	  `(set! ,name . ,value-or-body))
      `(set! ,(car name)
	     (lambda ,(cdr name)
	       (begin . ,value-or-body)))))


(set! next-gensym 0)
(define (gensym)
  (begin
    (set! next-gensym (+ next-gensym 1))
    (string->uninterned-symbol
     (concat "#" (number->string next-gensym)))))

(define-syntax (let bindings . body)
  `((lambda ,(map car bindings)
      (begin . ,body))
    . ,(map second bindings)))

;; We used map in our definition of let so we had better go ahead and
;; define that too. Note that these functions are not type-safe. We
;; should probably override them later with versions that are.
;;
;; Note the trick to confine the scope of iter. Normally we'd use
;; let/letrec here but their definition depends on map so we better not
;; touch them yet.
(define (reverse l)
  ((lambda (iter)
     (set! iter (lambda (in out)
		  (if (null? in)
		      out
		      (iter (cdr in) (cons (car in) out)))))

     (iter l nil)) nil))

(define (mapr fn lst)
  ((lambda (iter)
     (set! iter (lambda (done rest)
		  (if (null? rest)
		      done
		      (iter (cons (fn (car rest)) done) (cdr rest)))))

     (iter nil lst)) nil))

(define (map fn lst)
  (reverse (mapr fn lst)))

;; now we can define let* and letrec and we don't really have to do any
;; strange trickery anymore
(define-syntax (let* bindings . body)
  (if (null? bindings)
      `(begin . ,body)
      `(let (,(first bindings))
	 (let* ,(cdr bindings) . ,body))))

(define-syntax (letrec bindings . body)
  `(let ,(map (lambda (b) (list (first b) 'nil))
	      bindings)
     (begin
       . ,(map (lambda (b) (list 'set! (first b) (second b)))
	       bindings))
     (begin . ,body)))

(define (cadr x) (car (cdr x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (caddr x) (car (cddr x)))
(define (cdddr x) (cdr (cddr x)))
(define (cadddr x) (car (cdddr x)))
(define (cddddr x) (cdr (cdddr x)))
(define (caddddr x) (car (cddddr x)))

(define (first x) (car x))
(define (rest x) (cdr x))
(define (second x) (cadr x))
(define (third x) (caddr x))
(define (fourth x) (cadddr x))
(define (fifth x) (caddddr x))

(define (length=1 lst)
  (if (null? (car lst))
      #f
      (if (null? (cdr lst))
	  #t
	  #f)))

(define-syntax (push! obj dst)
  `(set! ,dst (cons ,obj ,dst)))

(define-syntax (pop! dst)
  `((lambda (top)
     (set! ,dst (cdr ,dst))
     top) (car ,dst)))

(define-syntax (inc! dst)
  `(set! ,dst (+ 1 ,dst)))

;; redefine define to include a docstring
(define define0 define)
(define-syntax (define name . value-or-body)
  (if (length=1 value-or-body)
      `(define0 ,name . ,value-or-body)
      (if (string? (car value-or-body))
	  (begin
	    (add-documentation (car name) (car value-or-body))
	    `(define0 ,name . ,(cdr value-or-body)))
	  `(define0 ,name . ,value-or-body))
      `(define0 ,name . ,value-or-body)))

(define define-syntax0 define-syntax)
(define-syntax (define-syntax name . value-or-body)
  (if (string? (car value-or-body))
    (add-documentation (car name) (car value-or-body)))
  `(define-syntax0 ,name . ,value-or-body))


(let ((docs nil))
  (define (add-documentation name string)
    (push! (cons name string) docs))

  ;; now that it's bound we can define it again
  ;; but with a docstring
  (define (add-documentation sym string)
    "add documentation to a symbol"
    (push! (cons sym string) docs))

  (define (get-documentation sym)
    "retrieve documentation for a symbol"
    (cdr (assoc sym docs))))

(define-syntax (doc name)
  "retrieve documentation for a name"
  `(get-documentation ',name))

(define-syntax (set-doc! name string)
  "create documentation for a name"
  `(add-documentation ',name ,string))

(set-doc! define-syntax
	  "create a new syntax procedure")

(define (index-of fn lst)
  "the first index for which fn evaluates true"
  (letrec ((iter (lambda (n rest)
		   (if (null? rest)
		       nil
		       (if (fn (car rest))
			   n
			   (iter (+ n 1) (cdr rest)))))))

    (iter 0 lst)))

(define (list-tail lst n)
  "the remainder of the list after calling cdr n times"
  (letrec ((iter (lambda (i rest)
		   (if (= i n)
		       rest
		       (iter (+ i 1) (cdr rest))))))

    (iter 0 lst)))

(define (list-ref lst n)
  "the car of the nth element of the list"
  (car (list-tail lst n)))

(define (index-eq val lst)
  "the index that is eq? to value"
  (index-of (lambda (x) (eq? x val)) lst))

(define (member-of eq val lst)
  "the remainder of the list that begins with val"
  (let ((idx (index-of
	      (lambda (item) (eq item val)) lst)))
    (when idx
	  (list-tail lst idx))))

(define (member obj lst)
  "the remainder of the list that begins with val. uses equal?"
  (member-of equal? obj lst))

(define (memq obj lst)
  "the remainder of the list that begins with val. uses eq?"
  (member-of eq? obj lst))

(define (compliment fn)
  "function that returns (not (fn))"
  (lambda (x) (not (fn x))))

;; now defining some standard conditional constructs
(define-syntax (not pred)
  `(if ,pred
       #f
       #t))

(define-syntax (when pred . conseq)
  "evaluates consequence if predicate evaluates true"
  `(if ,pred
       (begin . ,conseq)
       nil))

(define-syntax (unless pred . conseq)
  "evaluates consequence if predicate evaluates false"
  `(if ,pred
       nil
       (begin . ,conseq)))

(define-syntax (cond . clauses)
  "evaluates the cdr of the first clause whose car evaluates true"
  (if (null? clauses)
      #f
      (if (eq? (first (car clauses)) 'else)
	  `(begin . ,(rest (car clauses)))
	  (let ((result (gensym)))
	    `(let ((,result ,(first (car clauses))))
	       (if ,result
		   ,(if (rest (car clauses))
			`(begin . ,(rest (car clauses)))
			result)
		   (cond . ,(cdr clauses))))))))

(define-syntax (and . clauses)
  "evaluates clauses until one is false"
  (cond
   ((null? clauses) #t)
   ((length=1 clauses) (car clauses))
   (#t `(if ,(car clauses)
	    (and . ,(cdr clauses))
	    #f))))

(define-syntax (or . clauses)
  "evaluates clauses until one is true"
  (cond
   ((null? clauses) #f)
   ((length=1 clauses) (car clauses))
   (#t
    (let ((val (gensym)))
      `(let ((,val ,(car clauses)))
	 (if ,val
	     ,val
	     (or . ,(cdr clauses))))))))

(define-syntax (case key . clauses)
  "evaluates the first clause whose car contains key"
  (let ((key-val (gensym)))
    `(let ((,key-val ,key))
       (cond . ,(map (lambda (c)
		       (if (starts-with? c 'else eq?)
			   c
			   `((member? ,key-val ',(first c))
			     . ,(cdr c))))
		     clauses)))))

(define-syntax (funcall fn . args)
  `(,fn . ,args))

; enable that really cool rebinding let syntax that I've seen
; used but haven't found documented anywhere...
(define let0 let)
(define-syntax (let name-or-bindings . bindings-or-body)
  (if (symbol? name-or-bindings)
      (let0 ((name name-or-bindings)
	     (bindings (first bindings-or-body))
	     (body (rest bindings-or-body)))

        `(letrec ((,name
		   (lambda ,(map first bindings)
		     . ,body)))
	   (,name . ,(map second bindings))))
      `(let0 ,name-or-bindings . ,bindings-or-body)))

;; The exit-hook variable is looked up (in the current environment)
;; and invoked if set whenever the (exit) primitive function is
;; executed. The interpreter also invokes this hook when it's about to
;; abort due to an error condition.
;;
;; We'll install a hook that prints a backtrace and dumps the user
;; into a debug repl so that can try to figure out what went wrong
;; before things go down for good.
;;
;; Whenever the hook returns the interpreter will make the exit system
;; call for real and everything will disappear.
(define (print-backtrace)
  "print the backtrace from where we are. tail recursion hides some data"
  (letrec ((iter (lambda (rest)
		   (unless (null? rest)
			   (display (car rest))
			   (iter (cdr rest))))))

    (let ((cs (car callstack)))
      (iter (cdr (cdr cs))))))

(define (debug-repl)
  "repl that allows last-gasp debugging of a dying interpreter"
  ;(display "debug-repl>")
  (let ((result (eval (read-port stdin) base-env)))
    (write-port result stdout)
    (newline)
    (unless (eq? result 'quit)
	    (debug-repl))))

(define (exit-hook)
  "called as the last step of handling a hard interpreter exception"
  ;; turn off debug so that we don't get a lot of extra noise between
  ;; the real failure and the launch of the debug-repl.
  (set-debug! #f)

  ;; disable the exit-hook in case the thing that kicked us off was an
  ;; exception during writing out something that's going to appear in
  ;; the backtrace
  ;(set! exit-hook nil)

  ;; now dump an approximation of the backtrace (it's missing all
  ;; tail-calls) and launch a debug repl.
  ;(print-backtrace)
  ;(display "evaluate 'quit to exit")
  (debug-repl))

;; We want to also provide a way to exit without invoking the exit
;; hook. It seems natural to call this exit so we'll redefine the old
;; one and replace it.
(define throw-exit exit)

(define (throw-error . objs)
  (apply error objs)
  (throw-exit 1))

(define (exit val)
  "exit without activating the exit hook"
  (set! exit-hook nil)
  (throw-exit val))

(define (<=2 a b)
  (or (< a b) (= a b)))

(define-syntax (<= . values)
  (if (or (null? values)
	  (null? (cdr values)))
      #t
      `(and (<=2 ,(first values) ,(second values))
	    (<= . ,(cdr values)))))

;; Now go on and define a few useful higher level functions. This list
;; of things is largely driven by personal need at this point. Perhaps
;; I'll go back and try to implement whatever is in the spec more
;; closely.
(define (any? fn lst)
  "true if any of the list elements causes fn to evaluate true"
  (if (null? (index-of fn lst))
      #f
      #t))

(define (every? fn lst)
  "true if every list element causes fn to evaluate true"
  (letrec ((iter (lambda (rest)
		   (if (null? rest)
		       #t
		       (if (fn (car rest))
			   (iter (cdr rest))
			   #f)))))

    (iter lst)))

(define (member? val lst)
  "true if val is lst or if val is found eq? in list"
  (if (pair? lst)
      (if (null? (index-eq val lst))
	  #f
	  #t)
      (eq? val lst)))

(define (length items)
  "number of elements in a list"
  (letrec ((iter (lambda (a count)
		   (if (null? a)
		       count
		       (iter (cdr a) (+ 1 count))))))
    (iter items 0)))

(define (append-all lists)
  "append the lists inside the argument together end-to-end"
  (letrec ((iter (lambda (result current-list remaining-lists)
		   (if (null? current-list)
		       (if (null? remaining-lists)
			   result
			   (iter result
				 (car remaining-lists)
				 (cdr remaining-lists)))
		       (iter (cons (car current-list) result)
			     (cdr current-list)
			     remaining-lists)))))
    (reverse (iter nil nil lists))))

(define (append . lists)
  "append a series of lists together"
  (append-all lists))

(define (mappend fn lst)
  "map fn over list and append the resulting lists together"
  (append-all (map fn lst)))

(define (for-each f l)
  "call fn with each element of list"
  (if (null? l)
      #t
      (begin
	(f (car l))
	(for-each f (cdr l)))))

(define (read port)
  "read from a port"
  (read-port port))

(define load-eval eval)

(define (load name)
  "read and evaluate all forms in a file called name"
  (letrec ((in (open-input-port name))
	   (iter (lambda (form)
		   (unless (eof-object? form)
			   (write (load-eval form base-env))
			   (newline)
			   (iter (read-port in))))))
    (iter (read-port in))
    #t))

(let ((required nil))
  (letrec ((sym-to-name (lambda (name)
			  (if (symbol? name)
			      (string-append (symbol->string name) ".sch")
			      name))))
    (define (require name)
      "load file name if it hasn't already been loaded"
      (let ((name (sym-to-name name)))
	(unless (member? name required)
		(push! name required)
		(load name))))

    (define (provide sym)
      "declare a given symbol as being satisfied"
      (push! (sym-to-name sym) required)
      sym)))

(define (car-else obj alternate)
  "return (car obj) if it's not null"
  (if (null? obj)
      alternate
      (car obj)))

(define (newline . port)
  (let ((port (car-else port stdout)))
    (write-char #\newline port)))

(define (write-with-spaces port lst)
  (write-port (car lst) port)
  (unless (null? (cdr lst))
	  (write-char #\space port)
	  (write-with-spaces port (cdr lst))))

(define (write obj . port)
  (let ((p (car-else port stdout)))
    (write-port obj p)))

(define (display-string str port)
  "display a string without quotation marks"
  (let loop ((idx 0))
    (let ((char (string-ref str idx)))
      (unless (= (char->integer char) 0)
	      (write-char char port)
	      (loop (+ idx 1))))))

(define (number? obj)
  "is the object a kind of number?"
  (integer? obj)) ;; we only have 1 kind right now

(define (display obj . port)
  (let ((port (car-else port stdout)))
    (cond
     ((string? obj) (display-string obj port))
     ((number? obj) (display-string (number->string obj) port))
     ((char? obj) (write-char obj port))
     (else (write-port obj port)))))

(define (call-with-input-file file proc)
  (display "call-with-input-file: ")
  (display file)
  (newline)
  (let* ((in (open-input-port file))
	 (result (proc in)))
    (close-input-port in)
    result))

(define (call-with-output-file file proc)
  (display "call-with-output-file: ")
  (display file)
  (newline)
  (let* ((out (open-output-port file))
	 (result (proc out)))
    (close-output-port out)
    result))

(define (error . objs)
  (write-with-spaces stderr objs)
  (newline))

(define (peek-char port)
  (let ((ch (read-char port)))
    (unread-char port ch)
    ch))

(define (atom? obj)
  (not (pair? obj)))

(define (list? obj)
  (and (pair? obj)
       (or (null? (cdr obj))
	   (pair? (cdr obj)))))

(define (do-times fn times)
  (letrec ((iter (lambda (n)
		   (if (< n times)
		       (begin
			 (fn n)
			 (iter (+ n 1)))
		       #t))))
    (iter 0)))

(define-syntax (dotimes args . body)
  `(do-times (lambda (,(first args)) . ,body)
	     ,(second args)))

(define (find fn lst)
  (letrec ((iter (lambda (rest)
		   (if (null? rest)
		       #f
		       (let ((res (fn (car rest))))
			 (if res
			     (car rest)
			     (iter (cdr rest))))))))
    (iter lst)))

(define (filter fn lst)
  "create a new list that includes only the elements of lst for which fn evaluates true"
  (letrec ((iter (lambda (lst result)
		   (cond
		    ((null? lst) result)
		    ((fn (car lst))
		     (iter (cdr lst) (cons (car lst) result)))
		    (else (iter (cdr lst) result))))))
    (reverse (iter lst nil))))

(define filter-in filter)

(define (starts-with? exp val test)
  "true if a pair begins with val according to test"
  (and (pair? exp) (test (car exp) val)))

(define (assq key list)
  "find the first pair in list thats car is eq? to key"
  (find (lambda (e) (starts-with? e key eq?))
	list))

(define (assoc key list)
  "find the first pair in list thats car is eq? to key"
  (find (lambda (e) (starts-with? e key equal?))
	list))

(define (equal? a b)
  "true if each node in trees a and b are eq?"
  (cond
   ((and (pair? a) (pair? b))
    (and (equal? (car a) (car b))
	 (equal? (cdr a) (cdr b))))
   ((and (vector? a) (vector? b)
	 (= (vector-length a)
	    (vector-length b)))
    (let loop ((idx 0))
      (cond
       ((< idx (vector-length a))
	(if (= (vector-ref a idx)
	       (vector-ref b idx))
	    (loop (+ idx 1))
	    #f))
       (else #t))))
   (else (eq? a b))))

(define eqv? equal?)

(define (zero? val)
  (= val 0))

(define (char=? v1 v2)
  (eq? v1 v2))

(define (reduce fn lst . init)
  "apply fn to its previous result and each value of list. return final value"
  (letrec ((iter (lambda (last rest)
		   (if (null? rest)
		       last
		       (iter (fn last (car rest)) (cdr rest))))))
    (if (null? init)
	(iter (car lst) (cdr lst))
	(iter (car init) lst))))

;; originally found in tiny-clos but this was generally useful
;; enough to promote
(define (getl initargs name . not-found)
  "look for name in initargs and return the next thing in the list if it's found. return not-found otherwised"
  (letrec ((scan (lambda (tail)
		   (cond ((null? tail)
			  (if (pair? not-found)
			      (car not-found)
			      (error "GETL couldn't find" name)))
			 ((eq? (car tail) name) (cadr tail))
			 (else (scan (cddr tail)))))))
    (scan initargs)))

(define (string-append . args)
  "join a series of strings"
  (reduce concat args))

(define (string-length str)
  "find the length of a string, not including null"
  (let ((len 0))
    (while (not (= (char->integer (string-ref str len))
		   0))
      (inc! len))
    len))

(define (duplicate obj n)
  "create a list with obj duplicated n times"
  (let ((result nil))
    (dotimes (x n)
	     (set! result (cons obj result)))
    result))

(define (upto n)
  "creates a list from 0 to n-1 in increasing order"
  (let ((result nil))
    (dotimes (i n)
      (push! i result))
    (reverse result)))

(define (sort-list pred lst . equal)
  "arrange a list such that applying pred to any sequential pairs
returns true"
  (let ((equal (if (null? equal) equal?
		   (car equal))))
    (letrec ((pivot (lambda (l)
		      (cond ((null? l) 'done)
			    ((null? (cdr l)) 'done)
			    ((or (pred (car l) (cadr l))
				 (equal (car l) (cadr l)))
			     (pivot (cdr l)))
			    (else (car l)))))
	     (partition (lambda (piv l p1 p2)
			  (if (null? l) (list p1 p2)
			      (if (pred (car l) piv)
				  (partition piv (cdr l)
					     (cons (car l) p1)
					     p2)
				  (partition piv (cdr l)
					     p1
					     (cons (car l) p2))))))
	     (quicksort (lambda (l)
			  (let ((piv (pivot l)))
			    (if (eq? piv 'done) l
				(let ((parts (partition piv l nil nil)))
				  (append (quicksort (car parts))
					  (quicksort (cadr parts)))))))))

      (quicksort lst))))

(define-syntax (dowhile pred . body)
  "execute body whle pred evaluates true. checks pred after evaluating
body. always executes at least once"
  (let ((loop (gensym)))
    `(letrec
	 ((,loop
	   (lambda ()
	     (begin . ,body)
	     (when ,pred (,loop)))))

       (,loop))))

(define-syntax (while pred . body)
  "execute body whle pred evaluates true."
  (let ((loop (gensym))
	(cont? (gensym))
	(last (gensym)))
    `(let ,loop ((,cont? ,pred)
		 (,last nil))
	  (if ,cont?
	      (let ((,last (begin . ,body)))
		(,loop ,pred ,last))
	      ,last))))


(define-syntax (do bindings test-and-return . body)
  "establish and update bindings and execute body until test is true"
  (let ((loop (gensym)))
    `(let ,loop ,(map (lambda (binding)
		       (list (first binding)
			     (second binding)))
		     bindings)
	  (if ,(first test-and-return)
	      ,(if (rest test-and-return)
		   `(begin . ,(rest test-and-return)))
	      (begin
		,(if body
		     `(begin . ,body))
		(,loop . ,(map (lambda (binding)
				 (if (cddr binding)
				     (third binding)
				     (first binding)))
			       bindings)))))))


(define-syntax (dolist args . body)
  "evaluate body with (first args) taking successive values of (second args)"
  `(for-each (lambda (,(first args)) . ,body)
	     ,(second args)))

(define-syntax (dolist-idx args . body)
  "args is ((val idx) list), body evaluated with val and idx taking on successive values"
  `(let ((,(second (first args)) 0))
     (for-each
      (lambda (,(first (first args)))
	(begin . ,body)
	(inc! ,(second (first args))))
      ,(second args))))

(define (vector . args)
  "analogous to 'list' but for vectors"
  (let ((v (make-vector (length args) nil)))
    (dolist-idx ((val idx) args)
      (vector-set! v idx val))
    v))

(define-syntax (dovector args . body)
  "evaluate body for every element in a vector"
  (let ((n (gensym))
	(idx (gensym)))

    `(let ((,n (vector-length ,(second args))))
       (dotimes (,idx ,n)
		(let ((,(first args) (vector-ref ,(second args) ,idx)))
		  . ,body)))))

(define-syntax (assert cond)
  "verify that condition is true, throw error otherwise"
  (let ((result (gensym)))
    `(begin
       (let ((,result ,cond))
	 (if (not ,result)
	     (throw-error "assert failed" ',cond)
	     ,result)))))

;; Implement the classic delay/force combo directly by representing a
;; delay as the cons of its value (nil of not yet forced) and the
;; closure that computes it (nil if it has been forced)
(define-syntax (delay . body)
  "create a computation that can be completed later"
  `(cons nil (lambda () . ,body)))

(define (dynamic-wind before body after)
  "hack. do something useful here later"
  (before)
  (let ((result (body)))
    (after)
    result))

(define (force fn)
  "compute and or return the value of a delay"
  (when (and (not (null? (cdr fn))) (null? (car fn)))
	(set-car! fn ((cdr fn)))
	(set-cdr! fn nil))
  (car fn))

(define (all-symbols)
  "return a list of all symbols defined in the global environment"
  (append-all
   (map (lambda (x)
	  (cond
	   ((pair? x) nil)
	   ((hashtab? x) (hashtab-keys x))
	   (else (throw-error "giving up"))))
	base-env)))

(define-syntax (time . body)
  "display the time required to execute body"
  (let ((start (gensym))
	(result (gensym))
	(end (gensym)))
    `(let* ((,start (clock))
	    (,result (begin . ,body))
	    (,end (clock)))
       (for-each display
		 (list "execution took " (- ,end ,start)
		       "/" (clocks-per-sec) " seconds"))
       (newline)
       ,result)))

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
	 (define (,builder-sym . args)
	   ,(string-append "create a structure of type "
		    (symbol->string name))
	   (let ((struct (make-vector ,(+ 1 num-slots) nil)))
	     (vector-set! struct 0 ',name)

	     (dolist (slot ',slots)
	       (let ((val (member (car slot) args)))
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



(require 'math)

(provide 'stdlib)

