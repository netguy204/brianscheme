(define (dprintf . args)
  "debug printf"
  (apply printf args))

(define-struct lmbda
  "structure representing a processed lambda"
  (name
   env
   body
   args))

(define-struct lnode
  "a node in the ast"
  (type
   refs
   sets
   data))

(define-struct lvar
  "represents a variable"
  (name
   oname
   refs))

(define (make-environment syms structs)
  (cons syms structs))

(define (make-empty-environment)
  (make-environment nil nil))

(define (env-symbols env)
  (car env))

(define (env-structs env)
  (cdr env))

(define (new-lvar old-name)
  (make-lvar
   'name (gensym)
   'oname old-name
   'refs nil))

(define (extend-environment env vars)
  (let ((tvars (make-true-list vars)))
    (make-environment
      (cons tvars (env-symbols env))
      (cons (map new-lvar tvars) (env-structs env)))))

(define (lift exp)
  (lift:exp (macroexpand exp) nil))

(define (lift:exp exp env)
  (dprintf "lift:exp %a -- %a\n" exp env)
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
  (dprintf "lift:symbol %a -- %a\n" sym env)
  (let ((var (in-env? sym env)))
    (if var
	`(local ,(first var) ,(second var))
	`(global ,sym))))

(define (lift:begin exps env)
  (dprintf "lift:begin %a %a\n" exps env)
  (map (lambda (exp) (lift:exp exp env)) exps))

(define (lift:lambda args body env)
  (dprintf "lift:lambda %a, %a\n" args body)
  `(lambda ,args
     . ,(lift:begin body
		    (cons (make-true-list args)
			  env))))


