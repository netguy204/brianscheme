(define write-dbg write)
(define (write-dbg &rest args)
  #t)

(define (comp x env)
  (write-dbg 'comp x)
  (cond
   ((symbol? x) (gen-var x env))
   ((atom? x) (gen 'const x))
   ((syntax-procedure? x) (comp (macroexpand0 x) env))
   (else (case (first x)
	   (quote (gen 'const (second x)))
	   (begin (comp-begin (rest x) env))
	   (set! (seq (comp (third x) env) (gen-set (second x) env)))
	   (if (comp-if (second x) (third x) (rest (rest x)) env))
	   (lambda (gen 'fn (comp-lambda (second x) (rest (rest x)) env)))
	   (else (seq (mappend (lambda (y) (comp y env)) (rest x))
		      (comp (first x) env)
		      (gen 'call (length (rest x)))))))))

(define (comp-begin exps env)
  (write-dbg 'comp-begin exps)
  (cond ((null? exps) (gen 'const nil))
	((length=1 exps) (comp (first exps) env))
	(else (seq (comp (first exps) env)
		   (gen 'pop)
		   (comp-begin (rest exps) env)))))

(define (comp-if pred then else env)
  (write-dbg 'comp-if pred 'then then 'else else)
  (let ((l1 (gen-label))
	(l2 (gen-label)))
    (seq (comp pred env) (gen 'fjump l1)
	 (comp then env) (gen 'jump l2)
	 (list l1) (comp else env)
	 (list l2))))

(define (make-fn code env name args)
  (write-dbg 'make-fn 'code code)
  (list 'fn code env name args))

(define (fn? fn)
  (eq? (car fn) 'fn))

(define (fn-code fn)
  (first (cdr fn)))

(define (fn-env fn)
  (second (cdr fn)))

(define (fn-name fn)
  (third (cdr fn)))

(define (fn-args fn)
  (fourth (cdr fn)))

(define (assert-symbols lst)
  (unless (or (null? lst)
	      (and (pair? lst)
		   (every? symbol? lst)))
	  (error "lambda arglist must be list of symbols"
		 lst)
	  (exit 1)))

(define (comp-lambda args body env)
  (write-dbg 'comp-lambda args 'body body)
  (assert-symbols args)
  (let ((fst (gen 'args (length args)))
	(scd (comp-begin body (cons args env)))
	(thd (gen 'return)))
    (write-dbg 'first fst)
    (write-dbg 'second scd)
    (write-dbg 'third thd)
    (make-fn (seq fst scd thd)
	     env
	     "unknown"
	     args)))

(define label-num 0)

(define (compiler x)
  (set! label-num 0)
  (comp-lambda nil (list x) nil))

(define (gen opcode &rest args)
  (write-dbg 'gen opcode 'args args)
  (list (cons opcode args)))

(define (seq &rest code)
  (append-all code))

(define (gen-label)
  (write-dbg 'gen-label)
  (set! label-num (+ label-num 1))
  (string->symbol (concat "L" (number->string label-num))))
  
(define (gen-var var env)
  (write-dbg 'gen-var var)
  (let ((p (in-env? var env)))
    (if (not (null? p))
	(gen 'lvar (first p) (second p) ";" var)
	(gen 'gvar var))))

(define (gen-set var env)
  (write-dbg 'gen-set var)
  (let ((p (in-env? var env)))
    (if (not (null? p))
	(gen 'lset (first p) (second p) ";" var)
	(gen 'gset var))))

(define (in-env? symbol env)
  (let ((frame (find (lambda (f) (member? symbol f)) env)))
    (if (null? frame)
	nil
	(list (index-eq frame env) (index-eq symbol frame)))))

(define (show-fn fn)
  (if (not (fn? fn))
      (write "   " fn)
      (begin
	(newline)
	(dolist (instr (fn-code fn))
		(write instr)))))

(define (comp-show fn)
  (show-fn (compiler fn)))


; now we can compile functions to bytecode and print the results like
; this:
; (comp-show '(if (= x y) (f (g x)) (h x y (h 1 2))))

'compiler-loaded
