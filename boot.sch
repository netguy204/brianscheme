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

;; turn off profiling first so those numbers can be more useful later
(set-profiling! #f)

;; Some very basic defines to get us started

(set! nil '())

;; based on the macro bootstrap process in ericbb's javascript scheme
;; found at: http://norstrulde.org/
;; A more full featured cond will be defined later
(set! list (lambda x x))

(set! cond0
  (lambda clauses
    (if (null? clauses)
	nil
	(list 'if (car (car clauses))
	      (cons 'begin (cdr (car clauses)))
	      (cons 'cond0 (cdr clauses))))))
(set-macro! cond0)

(set! startswith
  (lambda (x sym)
    (eq? (car x) sym)))

;; more basic and that won't return the last expression they evaluate
(set! and0
  (lambda x
    (if (null? x)
	#t
	(list 'if (car x)
	      (cons 'and0 (cdr x))
	      #f))))
(set-macro! and0)

(set! not
  (lambda (x)
    (list 'if x #f #t)))
(set-macro! not)

(set! atom?
  (lambda (x)
    (not (pair? x))))

(set! append0
  (lambda (l1 l2)
    (cond0
     ((null? l1) l2)
     ((null? l2) l1)
     (#t (cons (car l1)
	       (append0 (cdr l1) l2))))))

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

(set! quasiquote (lambda (x) (qq x)))
(set-macro! quasiquote)

;; macro bootstrap complete. we can use quasiquoting syntax from here
;; on out!


(set! define-syntax
  (lambda (name-and-vars . body)
    `(begin
       (set! ,(car name-and-vars)
	     (lambda ,(cdr name-and-vars)
	       . ,body))
       (set-macro! ,(car name-and-vars)))))
(set-macro! define-syntax)

(define-syntax (define name . value-or-body)
  (if (symbol? name)
      (if (null? value-or-body)
	  `(set! ,name nil)
	  `(set! ,name . ,value-or-body))
      `(set! ,(car name)
	     (lambda ,(cdr name)
	       . ,value-or-body))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))
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

((lambda (next-gensym)
   (define (gensym)
     (set! next-gensym (%fixnum-add next-gensym 1))
     (string->uninterned-symbol
      (%prim-concat "G" (number->string next-gensym)))))
 0)

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

(define-syntax (%inc! dst)
  `(set! ,dst (%fixnum-add 1 ,dst)))

(define-syntax (inc! dst)
  `(set! ,dst (+ 1 ,dst)))

;; install a blank definition
(define (add-documentation name string)
  #t)

;; redefine define to include a docstring
(define define0 define)
(define-syntax (define name . value-or-body)
  (if (length=1 value-or-body)
      `(define0 ,name . ,value-or-body)
      (if (string? (car value-or-body))
	  (begin
	    (add-documentation (car name) (car value-or-body))
	    `(define0 ,name . ,(cdr value-or-body)))
	  `(define0 ,name . ,value-or-body))))

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

  ;; first working definition
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
     (else (loop (%fixnum-add n 1) (cdr remaining))))))

(define (list-tail lst n)
  "the remainder of the list after calling cdr n times"
  (let loop ((i 0)
	     (rest lst))
    (if (%fixnum-equal i n)
	rest
	(loop (%fixnum-add i 1) (cdr rest)))))

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
	(%inc! ,(second (first args))))
      ,(second args))))

(define-syntax (dovector args . body)
  "evaluate body for every element in a vector"
  (let ((n (gensym))
	(idx (gensym)))

    `(let ((,n (vector-length ,(second args))))
       (dotimes (,idx ,n)
		(let ((,(first args) (vector-ref ,(second args) ,idx)))
		  . ,body)))))


(define (do-times fn times)
  "call fn times with oone argument that goese from 0 to times-1"
  (assert (integer? times))
  (let loop ((n 0))
    (when (%fixnum-less-than n times)
      (fn n)
      (loop (%fixnum-add n 1))))
  #t)

(define-syntax (dotimes (idx max) . body)
  "execute body max times with idx going from 0 to max-1"
  `(do-times (lambda (,idx) . ,body) ,max))

(define (throw-error . objs)
  (apply error objs)
  (exit 1))

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
		       (iter (cdr a) (%fixnum-add 1 count))))))
    (iter items 0)))

(define (for-each f l)
  "call fn with each element of list"
  (if (null? l)
      #t
      (begin
	(f (car l))
	(for-each f (cdr l)))))

(define-syntax (car-else obj alternate)
  "return (car obj) if it's not null"
  `(if (null? ,obj)
       ,alternate
       (car ,obj)))

(define (read port)
  "read from a port"
  (read-port port))

(define (eof-object? obj)
  (eq? *eof-object* obj))

(define (file-exists?0 name)
  "Return #t if file (and only files) exists, otherwise false."
  (let ((port (open-input-port name)))
    (if (eof-object? port)
        #f
        (begin
          (close-input-port port)
          #t))))

(define (prim-concat str1 str2)
  (if (and (string? str1) (string? str2))
      (%prim-concat str1 str2)
      (throw-error "expected strings" str1 str2)))

(define (find-library name . paths)
  "Find the given library in the load path."
  (if (file-exists?0 name)
      name
      (let ((paths (car-else paths *load-path*)))
        (if (null? paths)
            #f
            (let ((file (prim-concat (car paths) (prim-concat "/" name))))
              (if (file-exists?0 file)
                  file
                  (find-library name (cdr paths))))))))

(define (load name)
  "read and evaluate all forms in a file called name"
  (let ((file (find-library name)))
    (if file
        (letrec ((in (open-input-port file))
                 (iter (lambda (form)
                         (unless (eof-object? form)
                           (eval form)
                           (iter (read-port in))))))
          (if (eof-object? in)
              (throw-error "failed to open" file)
              (iter (read-port in)))
          #t)
        (throw-error "could not find" name))))

(let ((required nil))
  (letrec ((sym-to-name (lambda (name)
			  (if (symbol? name)
			      (%prim-concat (symbol->string name) ".sch")
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
      (unless (%fixnum-equal (char->integer char) 0)
	      (write-char char port)
	      (loop (%fixnum-add idx 1))))))

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
    (%unread-char ch port)
    ch))

(define (atom? obj)
  (not (pair? obj)))

(define (list? obj)
  "true if obj appears to be a proper sublist"
  (and (pair? obj)
       (or (null? (cdr obj))
	   (pair? (cdr obj)))))

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
   ((eq? a b) #t)
   ((and (pair? a) (pair? b))
    (and (equal? (car a) (car b))
	 (equal? (cdr a) (cdr b))))
   ((and (vector? a) (vector? b)
	 (%fixnum-equal (vector-length a)
                        (vector-length b)))
    (let loop ((idx 0))
      (cond
       ((< idx (vector-length a))
	(if (equal? (vector-ref a idx)
                    (vector-ref b idx))
	    (loop (%fixnum-add idx 1))
	    #f))
       (else #t))))))

;; A-list functions.
(let ((ass (lambda (eqf key list)
             (find (lambda (e) (starts-with? e key eqf)) list)))
      (ass-set! (lambda (assf alist key value)
                  (let ((pair (assf key alist)))
                    (if pair
                        (begin
			  (set-cdr! pair value)
			  alist)
			(cons (cons key value) alist))))))

  (define (assq key list)
    "Find the first pair in list thats car is eq? to key."
    (ass eq? key list))

  (define (assv key list)
    "Find the first pair in list thats car is eqv? to key."
    (ass eqv? key list))

  (define (assoc key list)
    "Find the first pair in list thats car is equal? to key."
    (ass equal? key list))

  (define (assq-set! alist key value)
    "In ALIST, if KEY exists by eq? in ALIST assign KEY to VALUE."
    (ass-set! assq alist key value))

  (define (assv-set! alist key value)
    "In ALIST, if KEY exists by eqv? in ALIST assign KEY to VALUE."
    (ass-set! assv alist key value))

  (define (assoc-set! alist key value)
    "In ALIST, if KEY exists by equal? in ALIST assign KEY to VALUE."
    (ass-set! assoc alist key value)))

(define eqv? equal?)

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

(define (vector . args)
  "analogous to 'list' but for vectors"
  (let ((v (make-vector (length args) nil)))
    (dolist-idx ((val idx) args)
      (vector-set! v idx val))
    v))

(define-syntax (assert cond)
  "verify that condition is true, throw error otherwise"
  (let ((result (gensym)))
    `(begin
       (let ((,result ,cond))
	 (if (not ,result)
	     (throw-error "assert failed" ',cond)
	     ,result)))))

(define (union . lists)
  "compute the union of any number of lists"
  (letrec ((clean (lambda (list result)
		    (cond ((null? list) result)
			  ((memq (car list) result)
			   (clean (cdr list) result))
			  (else
			   (clean (cdr list) (cons (car list) result)))))))
    (clean (apply append lists) '())))

(define (remove-if test lst)
  "Remove elements matching predicate."
  (filter (complement test) lst))

(define (difference set subtracted)
  "compute the set difference (set - subtracted)"
  (remove-if (lambda (item)
	       (memq item subtracted)) set))


(define (all-symbols)
  "return a list of all symbols defined in the global environment"
  (hashtab-keys *global-environment*))

(define (bound? sym)
  "returns true if symbol is in the global environment"
  (let* ((sentinal (gensym))
	 (result (hashtab-ref *global-environment* sym sentinal)))
    (not (eq? result sentinal))))

(define (global-ref sym)
  "returns the global value of sym. error if not defined"
  (let* ((sentinal (gensym))
	 (result (if-compiling
		  ;; the compiled environment is slightly different
		  (cdr (hashtab-ref *global-environment* sym
				    (cons nil sentinal)))
		  (hashtab-ref *global-environment* sym sentinal))))
    (if (eq? result sentinal)
	(throw-error "symbol" sym "is not defined globally")
	result)))

(define (macro? sym)
  "is a given symbol defined as a global macro?"
  (and (bound? sym)
       (or (syntax-procedure? (global-ref sym))
	   (compiled-syntax-procedure? (global-ref sym)))))

(define (macroexpand0 form)
  "expand expression form by evaluating the macro at its head"
  (let ((fn (car form)))
    (if (macro? fn)
	(apply (global-ref fn) (cdr form))
	form)))

(define (macroexpand x)
  "fully expand all macros in form"
  (cond
   ((symbol? x) x)
   ((atom? x) x)
   (else
    (case (first x)
      (quote x)
      (begin `(begin . ,(map macroexpand (cdr x))))
      (set! `(set! ,(second x)
		   ,(macroexpand (third x))))
      (if `(if ,(macroexpand (second x))
	       ,(macroexpand (third x))
	       ,(macroexpand (fourth x))))
      (lambda `(lambda ,(second x)
		 . ,(map macroexpand (cddr x))))
      (else
       (if (comp-macro? (first x))
	   (macroexpand (macroexpand0 x))
	   `(,(macroexpand (first x)) .
	     ,(map macroexpand (cdr x)))))))))

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

(define (struct-constructor-name name)
  (string->symbol (%prim-concat "make-" (symbol->string name))))

(define (struct-predicate-name name)
  (string->symbol (%prim-concat (symbol->string name) "?")))

(define (struct-slot-getter-name name slot)
  (string->symbol (reduce %prim-concat (list (symbol->string name)
                                            "-" (symbol->string slot)
                                            "-ref"))))

(define (struct-slot-setter-name name slot)
  (string->symbol (reduce %prim-concat (list (symbol->string name)
                                            "-" (symbol->string slot)
                                            "-set!"))))


(define-syntax (define-struct name . docs-and-slots)
  "create a structure of a given name with a set of slots"
  (let ((slots (if (string? (car docs-and-slots))
		   (second docs-and-slots)
		   (first docs-and-slots))))
    `(begin
       (define (,(struct-constructor-name name) . args)
	 (let ((struct (make-vector ,(%fixnum-add (length slots) 1))))
	   (vector-set! struct 0 ',name)
	   ,@(let ((idx 0))
	       (map (lambda (slot)
		      (%inc! idx)
		      `(vector-set! struct ,idx
				    (getl args ',slot nil)))
		    slots))
	   struct))

       (define (,(struct-predicate-name name) struct)
	 (and (vector? struct) (eq? ',name (vector-ref struct 0))))

       . ,(let ((idx 0))
	    (map (lambda (slot)
		   (%inc! idx)
		   `(begin
		      (define (,(struct-slot-getter-name name slot) struct)
			(vector-ref struct ,idx))

		      (define (,(struct-slot-setter-name name slot) struct val)
			(vector-set! struct ,idx val))))
		 slots)))))



; Provides CL-style keyword arguments
(define (set-global-unquoted! var value)
  "In the global scope, set the symbol stored in VAR to VALUE."
  (eval `(set! ,var (quote ,value))))

(define (symbol->keyword sym)
  (let ((kw (string->symbol (%prim-concat ":" (symbol->string sym)))))
    (set-global-unquoted! kw kw)
    kw))

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
	    (let* (((varname default) arg)
		   (kwname (symbol->keyword varname))
		   (sym (gensym)))

	      (set! boa? #f)
	      (push! `(,varname (getl ,rest-sym ',kwname ,default))
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

(define-syntax (assert-types . types)
  "Check that types match expected types. Used when wrapping unsafe
%-functions with safe ones."
  (letrec ((add-check (lambda (lst)
                        (if (null? lst)
                            '()
                            (cons
                             `(unless ,(reverse (car lst))
                                (throw-error "expecting type"
                                             ,(caar lst)
                                             (quote ,(cadar lst))))
                             (add-check (rest lst)))))))
    (cons 'begin (add-check types))))

(define (set-car! lst value)
  "Set the car of the given cons."
  (assert-types (lst pair?))
  (%set-car! lst value))

(define (set-cdr! lst value)
  "Set the cdr of the given cons."
  (assert-types (lst pair?))
  (%set-cdr! lst value))

;; if-compiling is a special form in the compiler only. we define
;; syntax here so that if we're interpreting the else clause will
;; execute and if we're compiling the if clauses will execute (due to
;; the behavior of the special form).
(define-syntax (if-compiling conseq else)
  "execute conseq only if we're compiling. otherwise execute else"
  else)

(if-compiling
 ;; IF SIDE OF BRANCH: we're compiling this file for the first time,
 ;; we need to load and compile the compiler again but we'll need the
 ;; interpreter to do that
 (begin
   (display "Compiling compiler..." stderr)
   (write-char #\newline stdout)

   ;; we still need the interpreter to run the compiler until we get
   ;; the compiler compiled. we override compile file to make use of
   ;; the interpreted version of the compiler.

   (define (compile-file name)
     "read and compile all forms in file"
     (let ((file (find-library name)))
       (if file
           (letrec ((in (open-input-port file))
                    (iter (lambda (form)
                            (unless (eof-object? form)
                              ;;(write-port `((compiler ',form)) stdout)
                              (eval `((compiler ',form)))
                              (iter (read-port in))))))
             (if (eof-object? in)
                 (throw-error "failed to open" file)
                 (iter (read-port in)))
             #t)
           (throw-error "failed to find" name))))

   (compile-file "compiler.sch")

   ;; now the compiler is compiled so we can switch the
   ;; interpreter-hooked eval off for good
   (set! eval (lambda (form) ((compiler form))))

   (set! apply (new-fun
		'((args 2)
		  (lvar 0 1 ";" 'args)
		  (lvar 0 0 ";" 'fun)
		  (callj -1)
		  (return))
		nil nil nil))

   (set-cc-bytecode! (new-fun
		      '((args 1)
			(lvar 1 1)
			(lvar 1 0)
			(setcc)
			(lvar 0 0)
			(return))
		      nil nil nil))

   (set! call/cc (new-fun
		  '((args 1)
		    (cc)
		    (lvar 0 0)
		    (callj 1))
		  nil nil nil))

   (set! *vm-global-environment* *global-environment*)
   (define exit-hook comp-repl))

 ;; ELSE SIDE OF BRANCH: we're bootstrapping the world in the
 ;; interpreter. need to load up the compiler and start recompiling
 ;; the world
 (begin
   (display "Bootstrapping compiler..." stderr)
   (write-char #\newline stdout)

   (require 'compiler)
   (compile-file "boot.sch")
   'finished-compile))

;; once we reach this point we're fully bootstrapped and
;; compile-only. We simply return so that the native code that
;; originally kicked off the bootstrap can tear down the interpreter's
;; environment so that the memory can be reclaimed. It is the native
;; code that will then cause execution to continue by loading
;; stdlib.sch.
