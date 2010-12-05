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

(set! nil '())

(set! define0
      (macro (name vars body)
	`(set-local! ,name (lambda ,vars ,body))))
	
(set! define-syntax0
      (macro (name vars body)
	`(set! ,name (macro ,vars ,body))))


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
(set! car0 car)
(set! cdr0 cdr)

;; The first thing we'll be doing is defining the define-syntax
;; macro. We're going to need a basic let functionality first so lets
;; define that. This initial let0 is so-called because it doesn't
;; support automatically wrapping all of its arguments after the first
;; in an implicit (begin..). We'll need to add support for &rest
;; before we can make an appropriate let definition.

(set! next-gensym 0)
(define0 gensym ()
  (begin
    (set! next-gensym (+ next-gensym 1))
    (string->uninterned-symbol
     (concat "#" (number->string next-gensym)))))

(define-syntax0 let0 (bindings body)
  `((lambda ,(map car0 bindings)
      ,body)
    . ,(map second0 bindings)))

;; We used map in our definition of let0 so we had better go ahead and
;; define that too. Note that these functions are not type-safe. We
;; should probably override them later with versions that are.
(define0 reverse (l)
  (begin
    (define0 iter (in out)
      (if (pair? in)
	  (iter (cdr0 in) (cons (car0 in) out))
	  out))
    
    (iter l nil)))

(define0 mapr (fn lst)
  (begin
    (define0 iter (done rest)
      (if (null? rest)
	  done
	  (iter (cons (fn (car0 rest)) done) (cdr0 rest))))
    
    (iter nil lst)))

(define0 map (fn lst)
  (reverse (mapr fn lst)))

(define0 cadr0 (x) (car0 (cdr0 x)))
(define0 second0 (x) (cadr0 x))

(define0 cadr (x) (car (cdr x)))
(define0 caddr (x) (car (cdr (cdr x))))
(define0 cadddr (x) (car (cdr (cdr (cdr x)))))
(define0 caddddr (x) (car (cdr (cdr (cdr (cdr x))))))

(define0 first (x) (car x))
(define0 second (x) (cadr x))
(define0 third (x) (caddr x))
(define0 fourth (x) (cadddr x))
(define0 fifth (x) (caddddr x))

(define0 rest (x) (cdr x))

;; Defining some more convenience routines so that we can eventially
;; define the wrap-rest macro which will be very useful for defining
;; nicer versions of define-syntax and define that support &rest
;; arguments and allow the user to define functions that do the same.
(define0 index-of (fn lst)
  (begin
    (define0 iter (n rest)
      (if (null? rest)
	  nil
	  (if (fn (car0 rest))
	      n
	      (iter (prim-+ n 1) (cdr0 rest)))))
    (iter 0 lst)))

(define0 nth (lst n)
  (begin
    (define0 iter (i rest)
      (if (prim-= i n)
	  (car0 rest)
	  (iter (prim-+ i 1) (cdr0 rest))))
    (iter 0 lst)))

(define0 index-eq (val lst)
  (index-of (lambda (x) (eq? x val)) lst))

;; now building up function definition with &rest
(define-syntax0 wrap-rest (type args fbody)
  (let0 ((idx (index-eq '&rest args)))
	(if (null? idx)
	    `(,type ,args ,fbody)
	    `(,type ,args
		    (let0 ((,(nth args (prim-+ 1 idx)) (find-variable '&rest)))
			  ,fbody)))))


;; now we can define a proper define-syntax and use it to
;; build a proper define
(define-syntax0 define-syntax1 (name args fbody)
  `(set! ,name (wrap-rest macro ,args ,fbody)))

(define-syntax1 define-syntax (name args &rest fbody)
  `(set! ,name (wrap-rest macro ,args (begin . ,fbody))))

(define-syntax define (name &rest body)
  `(begin
     ,(if (symbol? name)
	  `(set-local! ,name nil)
	  `(set-local! ,(car0 name) nil))
     ,(if (symbol? name)
	  `(set! ,name . ,body)
	  `(set! ,(car0 name)
	     (wrap-rest lambda ,(cdr0 name) (begin . ,body))))))

;; Finally! Now we can get to work defining our standard conditional
;; constructs and other basic functionality that everyone expects to
;; have available.
(define-syntax not (pred)
  `(if ,pred
       #f
       #t))

(define (length=1 lst)
  (if (not (null? (car0 lst)))
      (if (null? (cdr0 lst))
	  #t
	  #f)
      #f))

(define-syntax let (bindings &rest body)
  `((lambda ,(map first bindings)
      (begin . ,body))
    . ,(map second bindings)))

(define-syntax let* (bindings &rest body)
  (if (null? bindings)
      `(begin . ,body)
      `(let (,(first bindings))
	 (let* ,(cdr bindings) . ,body))))

(define-syntax when (pred &rest conseq)
  `(if ,pred
       (begin . ,conseq)
       nil))

(define-syntax unless (pred &rest conseq)
  `(if ,pred
       nil
       (begin . ,conseq)))

(define-syntax cond (&rest clauses)
  (if (null? clauses)
      #f
      (if (eq? (first (car clauses)) 'else)
	  (second (car clauses))
	  `(if ,(first (car clauses))
	       (begin . ,(rest (car clauses)))
	       (cond . ,(cdr clauses))))))

(define-syntax and (&rest clauses)
  (cond
   ((null? clauses) #t)
   ((length=1 clauses) (car clauses))
   (#t `(if ,(car clauses)
	    (and . ,(cdr clauses))
	    #f))))

(define-syntax or (&rest clauses)
  (cond
   ((null? clauses) #f)
   ((length=1 clauses) (car clauses))
   (#t `(if ,(car clauses)
	    #t
	    (or . ,(cdr clauses))))))

(define-syntax case (key &rest clauses)
  (let ((key-val (gensym)))
    `(let ((,key-val ,key))
       (cond . ,(map (lambda (c)
		       (if (starts-with c 'else)
			   c
			   `((member? ,key-val ',(first c))
			     . ,(cdr c))))
		     clauses)))))

(define-syntax push! (obj dst)
  `(set! ,dst (cons ,obj ,dst)))

(define-syntax pop! (dst)
  `((lambda (top)
     (set! ,dst (cdr ,dst))
     top) (car ,dst)))

(define-syntax inc! (dst)
  `(set! ,dst (+ 1 ,dst)))

;; I'm pretty sure this is a fully legit version of apply. Let me know
;; if you disagree... I'm a little surprised this can be implemented
;; in userspace.
(define-syntax apply (fn args)
  `(,fn . ,(map (lambda (x) `',x) (eval args))))

(define-syntax funcall (fn &rest args)
  `(apply ,fn ',args))
 
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
  (define (iter rest)
    (unless (null? rest)
	    (write (car0 rest))
	    (iter (cdr0 rest))))
  (let ((cs (car0 callstack)))
    (iter (cdr0 (cdr0 cs)))))

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

(define-syntax throw-error (&rest objs)
  `(begin
     (error . ,objs)
     (throw-exit 1)))

(define (exit val)
  (set! exit-hook nil)
  (throw-exit val))

;; Now we start defining the type-safer versions of the primitives
;; that we started out with. We must be very careful in our
;; type-checking to only use code that doesn't use the same
;; type-checking methods. There are so many wonderful ways to mess
;; this up.
(define (assert-pair obj)
  (if (pair? obj)
      #t
      (throw-error "a pair was expected" obj)))

; This is a cute trick to verify num-args. If we obtain a list of
; values that begins at the last argument we expected to get then the
; cdr of that list better be nil.
(define-syntax assert-none-following (arg)
  `(if (eq? (cdr0 (find-variable ',arg)) nil)
       #t
       (throw-error ',arg "should be the last argument")))

(define (car lst)
  (assert-pair lst)
  (assert-none-following lst)

  (car0 lst))

(define (cdr lst)
  (assert-pair lst)
  (assert-none-following lst)

  (cdr0 lst))

(define set-car!0 set-car!)
(define (set-car! obj new-car)
  (assert-pair obj)
  (assert-none-following new-car)

  (set-car!0 obj new-car))

(define set-cdr!0 set-cdr!)
(define (set-cdr! obj new-cdr)
  (assert-pair obj)
  (assert-none-following new-cdr)

  (set-cdr!0 obj new-cdr))

(define (assert-numbers values)
  (if (any? (lambda (x) (not (integer? x))) values)
      (throw-error "tried to do math on non-integers" values)
      #t))

(define (+ &rest values)
  (assert-numbers values)
  (apply prim-+ values))

(define (- &rest values)
  (assert-numbers values)
  (apply prim-- values))

(define (* &rest values)
  (assert-numbers values)
  (apply prim-* values))

(define (< &rest values)
  (assert-numbers values)
  (apply prim-< values))

(define (> &rest values)
  (assert-numbers values)
  (apply prim-> values))

(define (= &rest values)
  (assert-numbers values)
  (apply prim-= values))

(define (<=2 a b)
  (or (< a b) (= a b)))

(define-syntax <= (&rest values)
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
  (assert-none-following name)

  (open-output-port0 name))

(define (close-output-port port)
  (assert-input-port port)
  (assert-none-following port)

  (close-output-port0 port))

; input ports
(define open-input-port0 open-input-port)
(define close-input-port0 close-output-port)

(define (open-input-port name)
  (assert-string 'open-input-port name)
  (assert-none-following name)

  (open-input-port0 name))

(define (close-input-port port)
  (assert-input-port port)
  (assert-none-following port)

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
  (define (iter rest)
    (if (null? rest)
	#t
	(if (fn (car rest))
	    (iter (cdr rest))
	    #f)))

  (iter lst))

(define (member? val lst)
  (if (pair? lst)
      (if (null? (index-eq val lst))
	  #f
	  #t)
      (eq? val lst)))

(define (length items)
  (define (iter a count)
    (if (null? a)
	count
	(iter (cdr a) (prim-+ 1 count))))
  (iter items 0))

(define (append-all lists)
  (define (iter result current-list remaining-lists)
    (if (null? current-list)
	(if (null? remaining-lists)
	    result
	    (iter result
		  (car remaining-lists)
		  (cdr remaining-lists)))
	(iter (cons (car current-list) result)
	      (cdr current-list)
	      remaining-lists)))
  (reverse (iter nil nil lists)))
	      
(define (append &rest lists)
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
  (let ((in (open-input-port name))
	(eval0 eval))
    (define (iter form)
      (unless (eof-object? form)
	      (write (eval0 form base-env))
	      (iter (read-port in))))
    (iter (read-port in))
    #t))

(define (newline)
  (write-char stdout #\newline))

(define (write-with-spaces port lst)
  (write-port port (car lst))
  (unless (null? (cdr lst))
	  (write-char port #\space)
	  (write-with-spaces port (cdr lst))))

(define (write &rest objs)
  (write-with-spaces stdout objs)
  (newline))

(define (error &rest objs)
  (write-with-spaces stderr objs)
  (newline))

(define (peek-char port)
  (let ((ch (read-char port)))
    (unread-char port ch)
    ch))

(define (atom? obj)
  (or (boolean? obj)
      (integer? obj)
      (char? obj)
      (string? obj)))

(define (do-times fn times)
  (define (iter n)
    (if (< n times)
	(begin
	  (fn n)
	  (iter (+ n 1)))
	#t))
  (iter 0))

(define-syntax dotimes (args &rest body)
  `(do-times (lambda (,(first args)) . ,body)
	     ,(second args)))

(define (starts-with lst val)
  (and (pair? lst)
       (eq? (first lst) val)))

(define (find fn lst)
  (define (iter rest)
    (if (null? rest)
	nil
	(let ((res (fn (car rest))))
	  (if res
	      (car rest)
	      (iter (cdr rest))))))
  (iter lst))

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

(define (reduce fn lst &rest init)
  (define (iter last rest)
    (if (null? rest)
	last
	(iter (fn last (car rest)) (cdr rest))))
  (if (null? init)
      (iter (car lst) (cdr lst))
      (iter (car init) lst)))

(define (duplicate obj n)
  (define result nil)
  (dotimes (x n)
	   (set! result (cons obj result)))
  result)

(define-syntax dolist (args &rest body)
  `(for-each (lambda (,(first args)) . ,body)
	     ,(second args)))


;; Implement the classic delay/force combo directly by representing a
;; delay as the cons of its value (nil of not yet forced) and the
;; closure that computes it (nil if it has been forced)
(define-syntax delay (&rest body)
  `(cons nil (lambda () . ,body)))

(define (force fn)
  (when (and (not (null? (cdr fn))) (null? (car fn)))
	(set-car! fn ((cdr fn)))
	(set-cdr! fn nil))
  (car fn))

;; For really simple performance testing: Print out the time it takes
;; to execute a set of forms.
(define-syntax time (&rest body)
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

