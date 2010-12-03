; this would get a bit noisy
(set! *debug* #f)

;; very basic define to get us started

(set! define0
      (macro (name vars body)
	'(set-local! ',name (lambda ,vars ,body))))
	
(set! define-syntax0
      (macro (name vars body)
	'(set! ,name (macro ,vars ,body))))

(set! nil '())

;; some primitives we override later that we need
;; to continue using in the lower levels 
(set! prim-+ +)
(set! prim-- -)
(set! prim-* *)
(set! prim-< <)
(set! prim-> >)
(set! prim-= =)
(set! car0 car)
(set! cdr0 cdr)

(set! next-gensym 0)
(define0 gensym ()
  (begin
    (set! next-gensym (+ next-gensym 1))
    (string->uninterned-symbol
     (concat "#" (number->string next-gensym)))))

;; primitive let since we don't have &rest yet
(define-syntax0 let0 (bindings body)
  '((lambda ,(map first bindings)
      ,body)
    . ,(map second bindings)))

;; handy primitives
(define0 map (fn lst)
  (begin
    (define0 iter (rest)
      (if (null? rest)
	  nil
	  (cons (fn (car0 rest)) (iter (cdr0 rest)))))
    (iter lst)))

(define0 not (x)
  (if x
      #f
      #t))

(define0 cadr (x) (car0 (cdr0 x)))
(define0 caddr (x) (car0 (cdr0 (cdr0 x))))
(define0 cadddr (x) (car0 (cdr0 (cdr0 (cdr0 x)))))

(define0 first (x) (car0 x))
(define0 second (x) (cadr x))
(define0 third (x) (caddr x))
(define0 fourth (x) (cadddr x))

(define0 rest (x) (cdr0 x))

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

(define0 length=1 (lst)
  (if (not (null? (car0 lst)))
      (if (null? (cdr0 lst))
	  #t
	  #f)
      #f))

;; now building up function definition with &rest
(define-syntax0 wrap-rest (type args fbody)
  (let0 ((idx (index-eq '&rest args)))
	(if (null? idx)
	    '(,type ,args ,fbody)
	    '(,type ,args
		    (let0 ((,(nth args (prim-+ 1 idx)) (find-variable '&rest)))
			  ,fbody)))))


;; now we can define a proper define-syntax and use it to
;; build a proper define
(define-syntax0 define-syntax1 (name args fbody)
  '(set! ,name (wrap-rest macro ,args ,fbody)))

(define-syntax1 define-syntax (name args &rest fbody)
  '(set! ,name (wrap-rest macro ,args (begin . ,fbody))))

(define-syntax let (bindings &rest body)
  '((lambda ,(map first bindings)
      (begin . ,body))
    . ,(map second bindings)))

(define-syntax let* (bindings &rest body)
  (if (null? bindings)
      '(begin . ,body)
      '(let (,(first bindings))
	 (let* ,(cdr bindings) . ,body))))


(define-syntax when (pred &rest conseq)
  '(if ,pred
       (begin . ,conseq)
       nil))

(define-syntax unless (pred &rest conseq)
  '(if ,pred
       nil
       (begin . ,conseq)))

(define-syntax cond (&rest clauses)
  (if (null? clauses)
      #f
      (if (eq? (first (car clauses)) 'else)
	  (second (car clauses))
	  '(if ,(first (car clauses))
	       ,(second (car clauses))
	       (cond . ,(cdr clauses))))))

(define-syntax and (&rest clauses)
  (cond
   ((null? clauses) #t)
   ((length=1 clauses) (car clauses))
   (#t '(if ,(car clauses)
	    (and . ,(cdr clauses))
	    #f))))


(define-syntax or (&rest clauses)
  (cond
   ((null? clauses) #f)
   ((length=1 clauses) (car clauses))
   (#t '(if ,(car clauses)
	    #t
	    (or . ,(cdr clauses))))))

(define-syntax push! (obj dst)
  '(set! ,dst (cons ,obj ,dst)))

(define-syntax pop! (dst)
  '((lambda (top)
     (set! ,dst (cdr ,dst))
     top) (car ,dst)))

(define-syntax define (name &rest body)
  '(begin
     ,(if (symbol? name)
	  '(set-local! ',name nil)
	  '(set-local! ',(car0 name) nil))
     ,(if (symbol? name)
	  '(set! ,name . ,body)
	  '(set! ,(first name)
	     (wrap-rest lambda ,(cdr0 name) (begin . ,body))))))

;; install the exit-hook
(define (print-backtrace)
  (define (iter rest)
    (unless (null? rest)
	    (write (car0 rest))
	    (iter (cdr0 rest))))
  (iter (car0 callstack)))

(define exit-hook print-backtrace)

;; repl for debugging
(define (repl)
  (write-port stdout 'debug-repl>)
  (let ((result (eval (read-port stdin) base-env)))
    (write-port stdout result)
    (write-char stdout #\newline)
    (unless (eq? result 'quit)
	    (repl))))

;; now that we have a proper define/-syntax we can keep going
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

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; todo, make tail recursive
(define (append-all lsts)
  (if (null? (rest lsts))
      (first lsts)
      (append (first lsts) (append-all (rest lsts)))))

(define (mappend fn lst)
  (append-all (map fn lst)))

(define (reverse l)
  (define (iter in out)
    (if (pair? in)
	(iter (cdr in) (cons (car in) out))
	out))
  (iter l '()))

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

(define-syntax throw-error (&rest objs)
  '(begin
     (error . ,objs)
     (exit 1)))

(define (peek-char port)
  (let ((ch (read-char port)))
    (unread-char port ch)
    ch))

;; now define type safe versions of the primitives
(define (assert-pair obj)
  (if (pair? obj)
      #t
      (throw-error "a pair was expected" obj)))

; cute trick to verify num-args. given the list of values
; that begins at the last argument, we expect the cdr to be
; nil
(define-syntax assert-none-following (arg)
  '(if (eq? (cdr0 (find-variable ',arg)) nil)
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

(define (atom? obj)
  (or (boolean? obj)
      (integer? obj)
      (char? obj)
      (string? obj)))


(define (assert-numbers values)
  (if (any? (lambda (x) (not (integer? x))) ',values)
      (throw-error "tried to do math on non-integers" ',values)
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

(define (do-times fn times)
  (define (iter n)
    (if (< n times)
	(begin
	  (fn n)
	  (iter (+ n 1)))
	#t))
  (iter 0))

(define-syntax delay (&rest body)
  '(cons nil (lambda () . ,body)))

(define (force fn)
  (when (and (not (null? (cdr fn))) (null? (car fn)))
	(set-car! fn ((cdr fn)))
	(set-cdr! fn nil))
  (car fn))

(define-syntax time (&rest body)
  (let ((start (gensym))
	(result (gensym))
	(end (gensym)))
    '(let* ((,start (clock))
	    (,result (begin . ,body))
	    (,end (clock)))
       (write "execution took" (- ,end ,start)
	      "/" (clocks-per-sec) "seconds")
       ,result)))

(define (starts-with lst val)
  (eq? (first lst) val))

(define-syntax case (key &rest clauses)
  (let ((key-val (gensym)))
    '(let ((,key-val ,key))
       (cond . ,(map (lambda (c)
		       (if (starts-with c 'else)
			   c
			   '((member? ,key-val ',(first c))
			     . ,(cdr c))))
		     clauses)))))

(define (find fn lst)
  (define (iter rest)
    (if (null? rest)
	nil
	(let ((res (fn (car rest))))
	  (if res
	      (car rest)
	      (iter (cdr rest))))))
  (iter lst))

(define-syntax dolist (args &rest body)
  '(for-each (lambda (,(first args)) . ,body)
	     ,(second args)))

'stdlib-loaded

;(set! *debug* #t)
