;; STAGE 2, bootstrapping from primitives.
;;
;; In this environment we don't have a working define or define-syntax
;; and all of the primitives do no type checking. We'll be building up
;; a more friendly environment from here.
;;
;; I'm using the fname0 notation to denote functions that are nearly
;; primitive and not intended for everyday use. None of these '0'
;; functions do type checking and often they are just renamed versions
;; of their original primitive self so that we can replace their name
;; with something more user-friendly.

;; Some very basic defines to get us started
(set-debug! #t)
(set! nil '())

(set! define-syntax
      (macro (name-and-vars . body)
	`(set! ,(car name-and-vars)
	       (macro ,(cdr name-and-vars)
		 (begin . ,body)))))

(define-syntax (define name . value-or-body)
  (if (symbol? name)
      `(set! ,name . ,value-or-body)
      `(set! ,(car name) (lambda ,(cdr name)
			   (begin . ,value-or-body)))))
	

;; We're going to override these names later so I'm stashing away the
;; original primitive versions so that we can refer to those
;; explicitely in the bootstrap environment. If we didn't do this
;; there would be a nasty circular dependency when things like
;; typesafe + went to check its arguments using functions implemented
;; in terms of typesafe +.
(set! prim-+ +)
(set! prim-- -)
(set! prim-* *)
(set! prim-< <)
(set! prim-> >)
(set! prim-= =)

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
     (define (iter in out)
       (if (null? in)
	   out
	   (iter (cdr in) (cons (car in) out))))
     
     (iter l nil)) nil))

(define (mapr fn lst)
  ((lambda (iter)
     (define (iter done rest)
       (if (null? rest)
	   done
	   (iter (cons (fn (car rest)) done) (cdr rest))))
    
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
(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (caddddr x) (car (cdr (cdr (cdr (cdr x))))))

(define (first x) (car x))
(define (rest x) (cdr x))
(define (second x) (cadr x))
(define (third x) (caddr x))
(define (fourth x) (cadddr x))
(define (fifth x) (caddddr x))

(define (index-of fn lst)
  (letrec ((iter (lambda (n rest)
		   (if (null? rest)
		       nil
		       (if (fn (car rest))
			   n
			   (iter (prim-+ n 1) (cdr rest)))))))

    (iter 0 lst)))

(define (nth-tail lst n)
  (letrec ((iter (lambda (i rest)
		   (if (prim-= i n)
		       rest
		       (iter (prim-+ i 1) (cdr rest))))))
		 
    (iter 0 lst)))

(define (nth lst n)
  (car (nth-tail lst n)))

(define (index-eq val lst)
  (index-of (lambda (x) (eq? x val)) lst))

(define (member val lst)
  (let ((idx (index-eq val lst)))
    (when idx
	  (nth-tail lst idx))))

(define (compliment fn)
  (lambda (x) (not (fn x))))

;; Now we get to work defining our standard conditional
;; constructs and other basic functionality that everyone expects to
;; have available.
(define-syntax (not pred)
  `(if ,pred
       #f
       #t))

(define (length=1 lst)
  (if (not (null? (car lst)))
      (if (null? (cdr lst))
	  #t
	  #f)
      #f))

(define-syntax (let bindings . body)
  `((lambda ,(map first bindings)
      (begin . ,body))
    . ,(map second bindings)))

(define-syntax (when pred . conseq)
  `(if ,pred
       (begin . ,conseq)
       nil))

(define-syntax (unless pred . conseq)
  `(if ,pred
       nil
       (begin . ,conseq)))

(define-syntax (cond . clauses)
  (if (null? clauses)
      #f
      (if (eq? (first (car clauses)) 'else)
	  (second (car clauses))
	  `(if ,(first (car clauses))
	       (begin . ,(rest (car clauses)))
	       (cond . ,(cdr clauses))))))

(define-syntax (and . clauses)
  (cond
   ((null? clauses) #t)
   ((length=1 clauses) (car clauses))
   (#t `(if ,(car clauses)
	    (and . ,(cdr clauses))
	    #f))))

(define-syntax (or . clauses)
  (cond
   ((null? clauses) #f)
   ((length=1 clauses) (car clauses))
   (#t `(if ,(car clauses)
	    #t
	    (or . ,(cdr clauses))))))

(define-syntax (case key . clauses)
  (let ((key-val (gensym)))
    `(let ((,key-val ,key))
       (cond . ,(map (lambda (c)
		       (if (starts-with c 'else)
			   c
			   `((member? ,key-val ',(first c))
			     . ,(cdr c))))
		     clauses)))))

(define-syntax (push! obj dst)
  `(set! ,dst (cons ,obj ,dst)))

(define-syntax (pop! dst)
  `((lambda (top)
     (set! ,dst (cdr ,dst))
     top) (car ,dst)))

(define-syntax (inc! dst)
  `(set! ,dst (+ 1 ,dst)))

;; I'm pretty sure this is a fully legit version of apply. Let me know
;; if you disagree... I'm a little surprised this can be implemented
;; in userspace.
(define-syntax (apply fn args)
  `(,fn . ,(map (lambda (x) `',x) (eval args))))

(define-syntax (funcall fn . args)
  `(,fn . ,args))
 
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
  (letrec ((iter (lambda (rest)
		   (unless (null? rest)
			   (write (car rest))
			   (iter (cdr rest))))))

    (let ((cs (car callstack)))
      (iter (cdr (cdr cs))))))

(define (debug-repl)
  (write-port stdout 'debug-repl>)
  (let ((result (eval (read-port stdin) base-env)))
    (write-port stdout result)
    (write-char stdout #\newline)
    (unless (eq? result 'quit)
	    (debug-repl))))

(define (exit-hook)
  ;; turn off debug so that we don't get a lot of extra noise between
  ;; the real failure and the launch of the debug-repl.
  (set-debug! #f)

  ;; disable the exit-hook in case the thing that kicked us off was an
  ;; exception during writing out something that's going to appear in
  ;; the backtrace
  (set! exit-hook nil)

  ;; now dump an approximation of the backtrace (it's missing all
  ;; tail-calls) and launch a debug repl.
  (print-backtrace)
  (write "evaluate 'quit to exit")
  (debug-repl))

;; We want to also provide a way to exit without invoking the exit
;; hook. It seems natural to call this exit so we'll redefine the old
;; one and replace it.
(define throw-exit exit)

(define-syntax (throw-error nil . objs)
  `(begin
     (error . ,objs)
     (throw-exit 1)))

(define (exit val)
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


(define (assert-string name var)
  (if (string? var)
      #t
      (throw-error "expected a string" var)))

(define (assert-input-port var)
  (if (input-port? var)
      #t
      (throw-error "expected an input port")))

(define (assert-output-port var)
  (if (output-port? var)
      #t
      (throw-error "expected an output port")))

; output ports
(define open-output-port0 open-output-port)
(define close-output-port0 close-output-port)

(define (open-output-port name)
  (assert-string 'open-output-port name)
  (open-output-port0 name))

(define (close-output-port port)
  (assert-output-port port)
  (close-output-port0 port))

; input ports
(define open-input-port0 open-input-port)
(define close-input-port0 close-output-port)

(define (open-input-port name)
  (assert-string 'open-input-port name)
  (open-input-port0 name))

(define (close-input-port port)
  (assert-input-port port)
  (close-input-port0 port))


;; Now go on and define a few useful higher level functions. This list
;; of things is largely driven by personal need at this point. Perhaps
;; I'll go back and try to implement whatever is in the spec more
;; closely.
(define (any? fn lst)
  (if (null? (index-of fn lst))
      #f
      #t))

(define (every? fn lst)
  (letrec ((iter (lambda (rest)
		   (if (null? rest)
		       #t
		       (if (fn (car rest))
			   (iter (cdr rest))
			   #f)))))

    (iter lst)))

(define (member? val lst)
  (if (pair? lst)
      (if (null? (index-eq val lst))
	  #f
	  #t)
      (eq? val lst)))

(define (length items)
  (letrec ((iter (lambda (a count)
		   (if (null? a)
		       count
		       (iter (cdr a) (prim-+ 1 count))))))
    (iter items 0)))

(define (append-all lists)
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
  (append-all lists))

(define (mappend fn lst)
  (append-all (map fn lst)))

(define (for-each f l)
  (if (null? l)
      #t
      (begin
	(f (car l))
	(for-each f (cdr l)))))

; read a single form from name
(define (read name)
  (let* ((in (open-input-port name))
	(result (read-port in)))
    (close-input-port in)
    result))

; read and evaluate all forms from name
(define (load name)
  (letrec ((in (open-input-port name))
	   (eval0 eval)
	   (iter (lambda (form)
		   (unless (eof-object? form)
			   (write (eval0 form base-env))
			   (iter (read-port in))))))
    (iter (read-port in))
    #t))

(define (newline)
  (write-char stdout #\newline))

(define (write-with-spaces port lst)
  (write-port port (car lst))
  (unless (null? (cdr lst))
	  (write-char port #\space)
	  (write-with-spaces port (cdr lst))))

(define (write . objs)
  (write-with-spaces stdout objs)
  (newline))

(define (error . objs)
  (write-with-spaces stderr objs)
  (newline))

(define (peek-char port)
  (let ((ch (read-char port)))
    (unread-char port ch)
    ch))

(define (atom? obj)
  (not (pair? obj)))

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

(define (starts-with lst val)
  (and (pair? lst)
       (eq? (first lst) val)))

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
  (letrec ((iter (lambda (lst result)
		   (cond
		    ((null? lst) result)
		    ((fn (car lst))
		     (iter (cdr lst) (cons (car lst) result)))
		    (else (iter (cdr lst) result))))))
    (reverse (iter lst nil))))

(define (starts-with? exp val)
  (and (pair? exp) (eq? (car exp) val)))

(define (assoc key list)
  (find (lambda (e) (starts-with? e key))
	list))

(define (equal? a b)
  (if (and (pair? a)
	   (pair? b))
      (and (equal? (car a) (car b))
	   (equal? (cdr a) (cdr b)))
      (eq? a b)))

(define (reduce fn lst . init)
  (letrec ((iter (lambda (last rest)
		   (if (null? rest)
		       last
		       (iter (fn last (car rest)) (cdr rest))))))
    (if (null? init)
	(iter (car lst) (cdr lst))
	(iter (car init) lst))))

(define (duplicate obj n)
  (let ((result nil))
    (dotimes (x n)
	     (set! result (cons obj result)))
    result))

(define-syntax (dolist args . body)
  `(for-each (lambda (,(first args)) . ,body)
	     ,(second args)))

(define-syntax (dovector args . body)
  (let ((n (gensym))
	(idx (gensym)))

    `(let ((,n (vector-length ,(second args))))
       (dotimes (,idx ,n)
		(let ((,(first args) (get-vector ,(second args) ,idx)))
		  . ,body)))))

;; Implement the classic delay/force combo directly by representing a
;; delay as the cons of its value (nil of not yet forced) and the
;; closure that computes it (nil if it has been forced)
(define-syntax (delay . body)
  `(cons nil (lambda () . ,body)))

(define (force fn)
  (when (and (not (null? (cdr fn))) (null? (car fn)))
	(set-car! fn ((cdr fn)))
	(set-cdr! fn nil))
  (car fn))

(define (all-symbols)
  (append-all
   (map (lambda (x)
	  (cond
	   ((pair? x) (car x))
	   ((hashtab? x) (keys-hashtab x))
	   (else (throw-error "giving up"))))
	base-env)))

;; For really simple performance testing: Print out the time it takes
;; to execute a set of forms.
(define-syntax (time . body)
  (let ((start (gensym))
	(result (gensym))
	(end (gensym)))
    `(let* ((,start (clock))
	    (,result (begin . ,body))
	    (,end (clock)))
       (write "execution took" (- ,end ,start)
	      "/" (clocks-per-sec) "seconds")
       ,result)))

(define (abs a)
  (if (< a 0)
      (- 0 a)
      a))

(define (min a b)
  (if (< a b)
      a
      b))

(define (max a b)
  (if (> a b)
      a
      b))

(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (- a (* b (/ a b))))))

(define (make-rat a b)
  (reduce-rat (cons a b)))

(define (numerator a)
  (car a))

(define (denominator a)
  (cdr a))

(define (reduce-rat rat)
  (let ((common (gcd (numerator rat) (denominator rat))))
    (cons (/ (numerator rat) common) (/ (denominator rat) common))))

(define (neg-rat rat)
  (make-rat (- 0 (numerator rat)) (denominator rat)))

(define (add-rat a b)
  (let ((na (numerator a))
	(nb (numerator b))
	(da (denominator a))
	(db (denominator b)))
    (make-rat (+ (* na db) (* nb da)) (* da db))))

(define (sub-rat a b)
  (add-rat a (neg-rat b)))

(define (mul-rat a b)
  (make-rat (* (numerator a) (numerator b))
	    (* (denominator a) (denominator b))))

(define (div-rat a b)
  (make-rat (* (numerator a) (denominator b))
	    (* (denominator a) (numerator b))))

'stdlib-loaded

