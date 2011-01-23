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
(set! + fixnum-add)
(set! - fixnum-sub)
(set! * fixnum-mul)
(set! / fixnum-div)
(set! < fixnum-less-than)
(set! > fixnum-greater-than)
(set! = fixnum-equal)

;; based on the macro bootstrap process in ericbb's javascript scheme
;; found at: http://norstrulde.org/
;; A more full featured cond will be defined later
(set! list (lambda x x))

(set! cond0
  (macro clauses
    (if (null? clauses)
	nil
	(list 'if (car (car clauses))
	      (cons 'begin (cdr (car clauses)))
	      (cons 'cond0 (cdr clauses))))))

(set! startswith
  (lambda (x sym)
    (eq? (car x) sym)))

;; more basic and that won't return the last expression they evaluate
(set! and0
  (macro x
    (if (null? x)
	#t
	(list 'if (car x)
	      (cons 'and0 (cdr x))
	      #f))))

(set! not
  (lambda (x)
    (if x #f #t)))

(set! atom?
  (lambda (x)
    (not (pair? x))))

(set! append0
  (lambda (x y)
    (cond
     ((null? x) y)
     ((null? y) x)
     (#t (cons (car x)
	       (append0 (cdr x) y))))))

(set! qq
  (lambda (x)
    (cond0
     ((atom? x)
      (list 'quote x))
     ((startswith x 'quote)
      (list 'list ''quote (qq (car (cdr x)))))
     ((startswith x 'unquote)
      (car (cdr x)))
     ((startswith x 'quasiquote)
      (qq (qq (car (cdr x)))))
     ((and0 (pair? (car x))
	    (startswith (car x) 'unquotesplicing))
      (list 'append0 (car (cdr (car x))) (qq (cdr x))))
     (#t
      (list 'cons (qq (car x)) (qq (cdr x)))))))

(set! quasiquote (macro (x) (qq x)))
;; macro bootstrap complete. we can use quasiquoting syntax from here
;; on out!


(set! define-syntax
      (macro (name-and-vars . body)
	`(set! ,(car name-and-vars)
	       (macro ,(cdr name-and-vars)
		 . ,body))))

(define-syntax (define name . value-or-body)
  (if (symbol? name)
      (if (null? value-or-body)
	  `(set! ,name nil)
	  `(set! ,name . ,value-or-body))
      `(set! ,(car name)
	     (lambda ,(cdr name)
	       . ,value-or-body))))

(set! next-gensym 0)
(define (gensym)
  (set! next-gensym (fixnum-add next-gensym 1))
  (string->uninterned-symbol
   (prim-concat "#" (number->string next-gensym))))

;; We used map in our definition of let0 so we had better go ahead and
;; define that early.
;;
;; Note the trick to confine the scope of iter. Normally we'd use
;; let/letrec here but their definition depends on map so we better
;; not touch them yet.
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

(define-syntax (let0 bindings . body)
  `((lambda ,(map car bindings)
      . ,body)
    . ,(map second bindings)))

;; now we can use let0 to define let*0 and letrec0
(define-syntax (let*0 bindings . body)
  (if (null? bindings)
      `(begin . ,body)
      `(let0 (,(first bindings))
	 (let*0 ,(cdr bindings) . ,body))))

(define-syntax (letrec0 bindings . body)
  `(let0 ,(map (lambda (b) (list (first b) 'nil))
	      bindings)
     ,@(map (lambda (b) (list 'set! (first b) (second b)))
	       bindings)
     . ,body))

;; now we define everything we need so that we can enable the real
;; destructuring versions of let and the named-let form
(define-syntax (push! obj dst)
  `(set! ,dst (cons ,obj ,dst)))

(define-syntax (pop! dst)
  `((lambda (top)
     (set! ,dst (cdr ,dst))
     top) (car ,dst)))

(define-syntax (cond . clauses)
  "evaluates the cdr of the first clause whose car evaluates true"
  (if (null? clauses)
      #f
      (if (eq? (first (car clauses)) 'else)
	  `(begin . ,(rest (car clauses)))
	  (let0 ((result (gensym)))
	    `(let0 ((,result ,(first (car clauses))))
	       (if ,result
		   ,(if (rest (car clauses))
			`(begin . ,(rest (car clauses)))
			result)
		   (cond . ,(cdr clauses))))))))

(define (append-all lists)
  "append the lists inside the argument together end-to-end"
  (letrec0 ((iter (lambda (result current-list remaining-lists)
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

(define (destructure-into-bindings var-forms value-forms)
  "produces let style binding pairs with operations on value-forms
that decompose it according to the structure of var-forms"
  (letrec0 ((result nil)
	    (iter
	     (lambda (vars values)
	       (cond
		((symbol? vars)
		 (push! (list vars values) result))
		((pair? vars)
		 (iter (car vars) `(car ,values))
		 (iter (cdr vars) `(cdr ,values)))
		((null? vars) #t)
		(else (throw-error "don't know what to do with" vars))))))
    (iter var-forms value-forms)
    (reverse result)))

(define (destructure-all-bindings bindings)
  (mappend (lambda (binding)
	     (destructure-into-bindings (first binding)
					(second binding)))
	   bindings))

;; Now we're finally ready to enable destructuring
;; named-let/let*/letrec
(define-syntax (let name-or-bindings . bindings-or-body)
  (if (symbol? name-or-bindings)
      (let0 ((name name-or-bindings)
	     (bindings (destructure-all-bindings (first bindings-or-body)))
	     (body (rest bindings-or-body)))

        `(letrec0 ((,name
		   (lambda ,(map first bindings)
		     . ,body)))
	   (,name . ,(map second bindings))))
      `(let0 ,(destructure-all-bindings name-or-bindings) . ,bindings-or-body)))

(define-syntax (let* bindings . body)
  `(let*0 ,(destructure-all-bindings bindings) . ,body))

(define-syntax (letrec bindings . body)
  `(letrec0 ,(destructure-all-bindings bindings) . ,body))

(define (cadr x) (car (cdr x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (caddr x) (car (cddr x)))
(define (cdddr x) (cdr (cddr x)))
(define (cadddr x) (car (cdddr x)))
(define (cddddr x) (cdr (cdddr x)))
(define (caddddr x) (car (cddddr x)))

(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define fifth caddddr)

(define (length=1 lst)
  (if (null? (car lst))
      #f
      (if (null? (cdr lst))
	  #t
	  #f)))

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

;; redefined define-syntax to include a docstring and argument
;; destructuring
(define define-syntax0 define-syntax)
(define-syntax (define-syntax name-and-args . maybe-doc-and-body)
  (if (string? (car maybe-doc-and-body))
    (add-documentation (car name-and-args) (car maybe-doc-and-body)))
  (let ((args (gensym)))
    `(define-syntax0 (,(car name-and-args) . ,args)
       (let ((,(cdr name-and-args) ,args))
	 . ,maybe-doc-and-body))))

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
    (let ((result (assoc sym docs)))
      (if result
	  (cdr result)
	  ""))))

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
  (let loop ((n 0)
	     (remaining lst))
    (cond
     ((null? remaining) nil)
     ((fn (car remaining)) n)
     (else (loop (+ n 1) (cdr remaining))))))

(define (list-tail lst n)
  "the remainder of the list after calling cdr n times"
  (let loop ((i 0)
	     (rest lst))
    (if (= i n)
	rest
	(loop (+ i 1) (cdr rest)))))

(define (list-ref lst n)
  "the car of the nth element of the list"
  (car (list-tail lst n)))

(define (index-eq val lst)
  "the index that is eq? to value"
  (index-of (lambda (x) (eq? x val)) lst))

(define (member-of eq val lst)
  "the remainder of the list that begins with val"
  (let loop ((remaining lst))
    (cond
     ((null? remaining) nil)
     ((eq (car remaining) val) remaining)
     (else (loop (cdr remaining))))))

(define (member obj lst)
  "the remainder of the list that begins with val. uses equal?"
  (member-of equal? obj lst))

(define (memq obj lst)
  "the remainder of the list that begins with val. uses eq?"
  (member-of eq? obj lst))

(letrec ((del (lambda (eqf item list)
		(if (null? list)
		    list
		    (if (eqf (first list) item)
			(del eqf item (rest list))
			(cons (first list)
			      (del eqf item (rest list))))))))

  (define (delq item list)
    "Return list with all items eq? to item removed."
    (del eq? item list))

  (define (delv item list)
    "Return list with all items eqv? to item removed."
    (del eqv? item list))

  (define (delete item list)
    "Return list with all items equal? to item removed."
    (del equal? item list)))

(define (compliment fn)
  "function that returns (not (fn))"
  (lambda (x) (not (fn x))))

;; now defining some standard conditional constructs
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
		       (cond
			((starts-with? c 'else eq?) c)
			((pair? (first c))
			 `((memq ,key-val ',(first c)) . ,(cdr c)))
			(else `((eq? ,key-val ',(first c)) . ,(cdr c)))))
		     clauses)))))

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
  (let ((result (eval (read-port stdin))))
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

(define (every-pair? pred lst)
  "true if binary-pred is true for every pair as it slides down the
list"
  (let loop ((remaining lst))
    (cond
     ((null? remaining) #t)
     ((null? (cdr remaining)) #t)
     ((pred (first remaining)
	    (second remaining)) (loop (cdr remaining)))
     (else #f))))

(define (<=2 a b)
  (or (< a b) (= a b)))

(define (<= . values)
  (every-pair? <=2 values))

(define (>=2 a b)
  (or (> a b) (= a b)))

(define (>= . values)
  (every-pair? >=2 values))

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
  (let loop ((remaining lst))
    (cond
     ((null? remaining) #t)
     ((fn (car remaining)) (loop (cdr remaining)))
     (else #f))))

(define (member? val lst)
  "true if val is lst or if val is found eq? in list"
  (and (memq val lst) #t))

(define (length items)
  "number of elements in a list"
  (letrec ((iter (lambda (a count)
		   (if (null? a)
		       count
		       (iter (cdr a) (+ 1 count))))))
    (iter items 0)))

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

;; the definition of eval that load will used. this is convenient so
;; that we can override the behavior of load later if we wish
(define load-eval eval)

(define (load name)
  "read and evaluate all forms in a file called name"
  (letrec ((in (open-input-port name))
	   (iter (lambda (form)
		   (unless (eof-object? form)
			   (write (load-eval form))
			   (newline)
			   (iter (read-port in))))))
    (if (eof-object? in)
	(throw-error "failed to open" name)
	(iter (read-port in)))
    #t))

(let ((required nil))
  (letrec ((sym-to-name (lambda (name)
			  (if (symbol? name)
			      (string-append (symbol->string name) ".sch")
			      name))))
    (define (require name)
      "load file name if it hasn't already been loaded"
      (let ((name (sym-to-name name)))
	(unless (memq name required)
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
  "write a newline to port (defaults to stdout)"
  (let ((port (car-else port stdout)))
    (write-char #\newline port)))

(define (write-with-spaces port lst)
  "write a series of forms to port"
  (write-port (car lst) port)
  (unless (null? (cdr lst))
	  (write-char #\space port)
	  (write-with-spaces port (cdr lst))))

(define (display-with-spaces . args)
  "write a series of forms to stdout"
  (write-with-spaces stdout args))

(define (write obj . port)
  "write a form to port (defaults to stdout)"
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
  (or (real? obj) (integer? obj)))

(define (display obj . port)
  "write form to port (stdout default) in display format. strings will
not be quoted or escaped."
  (let ((port (car-else port stdout)))
    (cond
     ((string? obj) (display-string obj port))
     ((number? obj) (display-string (number->string obj) port))
     ((char? obj) (write-char obj port))
     (else (write-port obj port)))))

(define (call-with-input-file file proc)
  "open file and pass the port to proc, close when proc returns"
  (let* ((in (open-input-port file))
	 (result (proc in)))
    (close-input-port in)
    result))

(define (call-with-output-file file proc)
  "open file and pass the port to proc, close when proc returns"
  (let* ((out (open-output-port file))
	 (result (proc out)))
    (close-output-port out)
    result))

(define (error . objs)
  "write forms to stderr"
  (write-with-spaces stderr objs)
  (newline))

(define (peek-char port)
  (let ((ch (read-char port)))
    (unread-char port ch)
    ch))

(define (atom? obj)
  (not (pair? obj)))

(define (list? obj)
  "true if obj appears to be a proper sublist"
  (and (pair? obj)
       (or (null? (cdr obj))
	   (pair? (cdr obj)))))

(define (do-times fn times)
  "call fn times with oone argument that goese from 0 to times-1"
  (let loop ((n 0))
    (when (< n times)
      (fn n)
      (loop (+ n 1))))
  #t)

(define-syntax (dotimes (idx max) . body)
  "execute body max times with idx going from 0 to max-1"
  `(do-times (lambda (,idx) . ,body) ,max))

(define (find pred lst)
  "return the first element of lst for which pred is true. #f if none"
  (let loop ((remainder lst))
    (cond
     ((null? remainder) #f)
     ((pred (car remainder)) (car remainder))
     (else (loop (cdr remainder))))))

(define (filter fn lst)
  "create a new list that includes only the elements of lst for which fn evaluates true"
  (let loop ((remainder lst)
	     (result nil))
    (cond
     ((null? remainder) (reverse result))
     ((fn (car remainder))
      (loop (cdr remainder)
	    (cons (car remainder) result)))
     (else (loop (cdr remainder) result)))))

(define filter-in filter)

(define (starts-with? exp val test)
  "true if a pair begins with val according to test"
  (and (pair? exp) (test (car exp) val)))

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

(define (assq key list)
  "find the first pair in list thats car is eq? to key"
  (find (lambda (e) (starts-with? e key eq?))
	list))

(define (assoc key list)
  "find the first pair in list thats car is eq? to key"
  (find (lambda (e) (starts-with? e key equal?))
	list))

(define eqv? equal?)

(define (zero? val)
  (= val 0))

(define (char=? v1 v2)
  (eq? v1 v2))

(define (reduce fn lst . init)
  "apply fn to its previous result and each value of list. return
final value"
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
  "look for name in initargs and return the next thing in the list if
it's found. return not-found otherwised"
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
  (reduce prim-concat args))

(define (string-length str)
  "find the length of a string, not including null"
  (let ((len 0))
    (while (not (= (char->integer (string-ref str len))
		   0))
      (inc! len))
    len))

(define (substring str start end)
  "Return given substring from start (inclusive) to end (exclusive)."
  (let* ((strlen (string-length str))
	 (end (or end strlen))
	 (len (- end start)))
    (if (or (> len strlen) (> end strlen) (> start (- strlen 1)))
	(throw-error "out of string bounds" str start end))
    (if (> start end)
	(throw-error "invalid substring" start end))
    (let ((substr (make-string len #\.)))
      (dotimes (i len)
        (string-set! substr i (string-ref str (+ i start))))
      substr)))

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
  (hashtab-keys *global-environment*))

(define (global? sym)
  "returns true if symbol is in the global environment"
  (let* ((sentinal (gensym))
	 (result (hashtab-ref *global-environment* sym sentinal)))
    (not (eq? result sentinal))))

(define (global-ref sym)
  "returns the global value of sym. error if not defined"
  (let* ((sentinal (gensym))
	 (result (hashtab-ref *global-environment* sym sentinal)))
    (if (eq? result sentinal)
	(throw-error "symbol" sym "is not defined globally")
	result)))

(define (macro? sym)
  "is a given symbol defined as a global macro?"
  (and (global? sym)
       (syntax-procedure? (global-ref sym))))

(define (macroexpand0 form)
  "expand expression form by evaluating the macro at its head"
  (let ((fn (car form)))
    (if (macro? fn)
	(apply (global-ref fn) (cdr form))
	form)))

(define (macroexpand form)
  "macroexpand the expression in form fully"
  (cond
   ((not (pair? form)) form)
   (else
    (let ((expansion (macroexpand0 form)))
      (if (equal? expansion form)
	  ;; head is fully expanded, now expand the rest
	  (cons (first expansion) (map macroexpand (rest expansion)))
	  ;; head may be able to expand further
	  (macroexpand expansion))))))


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


;; create a facility that provides something like dynamic variables in
;; a lexical-only scheme. It works by making closure with the name of
;; the dynamic variable and by maintaining a stack in a hashtable. To
;; use this:
;;
;; (defvar *foo* 42)
;;
;; (*foo*)  ;; yields 42
;;
;; (binding ((*foo* "bar"))
;;   (*foo*)) ;; yields "bar"
;;
;; (define (other) (+ (*foo*) 1))
;; (other) ;; yields 43
;; (binding ((*foo* 1))
;;   (other)) ;; yields 2
;;
;; The binding for *foo* is global in the sense that all code executed
;; in or through the forms executed in the scope of a binding form
;; will see the values established by that form.
(let ((dynvars (make-hashtab-eq 30)))
  (define (create-dynamic-binding symbol init)
    (hashtab-set!
     dynvars symbol
     (let ((value (list init)))
       (list (lambda () ;; getter
	       (first value))
	     (lambda (val) ;; pusher
	       (push! val value))
	     (lambda () ;; popper
	       (pop! value))))))

  (define (dynamic-binding? sym)
    (hashtab-ref dynvars sym nil))

  (define (get-binding-getter sym)
    (first (hashtab-ref dynvars sym)))

  (define (get-binding-setter sym)
    (second (hashtab-ref dynvars sym)))

  (define (get-binding-popper sym)
    (third (hashtab-ref dynvars sym))))

;; now the methods the usual methods the user will use
(define-syntax (defvar name init)
  "create a new dynamic binding"
  `(begin (create-dynamic-binding ',name ,init)
	  (set! ,name (get-binding-getter ',name))))

(define-syntax (binding forms . body)
  "bind the dynamic variables within the scope of body"
  (let ((result (gensym)))
    `(begin
       (begin . ,(map (lambda (form)
			`((get-binding-setter ',(first form))
			  ,(second form)))
		      forms))
       (let ((,result (begin . ,body)))
	 (begin . ,(map (lambda (form)
			  `((get-binding-popper ',(first form))))
			forms))
	 ,result))))

(require 'clos)
(require 'math)
(provide 'stdlib)

(require 'clos-repl)
