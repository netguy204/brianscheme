(define (comp x env)
  (cond
   ((symbol? x) (gen-var x env))
   ((atom? x) (gen 'const x))
   ((syntax-procedure? x) (comp (macroexpand0 x) env))
   ((case (first x)
      (quote (gen 'const (second x)))
      (begin (comp-begin (rest x) env))
      (set! (seq (comp (third x) env) (gen-set (second x) env)))
      (if (comp-if (second x) (third x) (rest (rest x)) env))
      (lambda (gen 'fn (comp-lambda (second x) (rest (rest x)) env)))
      ;; apply procedure
      (else (seq (mappend (lambda (y) (comp y env)) (rest x))
		 (comp (first x) env)
		 (gen 'call (length (rest x)))))))))

(define (comp-begin exps env)
  (cond ((null? exps) (gen 'const nil))
	((length=1 exps) (comp (first exps) env))
	(else (seq (comp (first exps) env)
		   (gen 'pop)
		   (comp-begin (rest exps) env)))))

(define (comp-if pred then else env)
  (let ((l1 (gen-label))
	(l2 (gen-label)))
    (seq (comp pred env) (gen 'fjump l1)
	 (comp then env) (gen 'jump l2)
	 (list l1) (comp else env)
	 (list l2))))

(define (make-fn code env name args)
  (list code env name args))

(define (fn-code fn)
  (first fn))

(define (fn-env fn)
  (second fn))

(define (fn-name fn)
  (third fn))

(define (fn-args fn)
  (fourth fn))

(define (assert-symbols lst)
  (unless (or (null? lst)
	      (and (pair? lst)
		   (every? symbol? lst)))
	  (error "lambda arglist must be list of symbols"
		 lst)
	  (exit 1)))

(define (comp-lambda args body env)
  (assert-symbols args)
  (make-fn (seq (gen 'args (length args))
		(comp-begin body (cons args env))
		(gen 'return))
	   env
	   "unknown"
	   args))

(define label-num 0)

(define (compiler x)
  (set! label-num 0)
  (comp-lambda '() (list x) nil))

(define (gen opcode &rest args)
  (list (cons opcode args)))

(define (seq &rest code)
  (append-all (append-all code)))

(define (gen-label)
  (set! label-num (+ label-num 1))
  (string->symbol (concat "L" (number->string label-num))))
  
(define (gen-var var env)
  (let ((p (in-env? var env)))
    (if (not (null? p))
	(gen 'lvar (first p) (second p) ";" var)
	(gen 'gvar var))))

(define (gen-set var env)
  (let ((p (in-env? var env)))
    (if (not (null? p))
	(gen 'lset (first p) (second p) ";" var)
	(gen 'gset var))))

(define (in-env? symbol env)
  (let ((frame (find (lambda (f) (member? symbol f)) env)))
    (if (null? frame)
	nil
	(list (index-eq frame env) (index-eq symbol frame)))))


;; now test stuff
(set! env '((a b c) (d e f) (g h i)))
;(set! *debug* #t)
;(in-env? 'e env)
