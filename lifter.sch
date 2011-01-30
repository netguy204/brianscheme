
(define (lift exp)
  (lift:exp (comp-macroexpand exp) nil))

(define (lift:exp exp env)
  (printf "lift:exp %a -- %a\n" exp env)
  (cond
   ((symbol? exp)
    (lift:symbol exp env))
   ((atom? exp) exp)
   (else
    (case (first exp)
      (quote exp)

      (begin
	`(begin . ,(lift:begin (cdr exp) env)))

      (set!
       (list 'set!
	     (second exp)
	     (lift:exp (third exp) env)))

      (if
       `(if ,(lift:exp (second exp) env)
	    ,(lift:exp (third exp) env)
	    ,(lift:exp (fourth exp) env)))

      (lambda
	  (lift:lambda (second exp)
		       (cddr exp)
		       env))

      (else
       (cons
	(lift:exp (first exp) env)
	(map (lambda (e) (lift:exp e env))
	     (cdr exp))))))))


(define (lift:symbol sym env)
  (printf "lift:symbol %a -- %a\n" sym env)
  (let ((var (in-env? sym env)))
    (if var
	`(local ,(first var) ,(second var))
	`(global ,sym))))

(define (lift:begin exps env)
  (printf "lift:begin %a %a\n" exps env)
  (map (lambda (exp) (lift:exp exp env)) exps))

(define (lift:lambda args body env)
  (printf "lift:lambda %a, %a\n" args body)
  `(lambda ,args
     . ,(lift:begin body
		    (cons (make-true-list args)
			  env))))


