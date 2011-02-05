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
   children))

(define-struct lvar
  "represents a variable"
  (name
   oname
   refs))

(define (new-lnode type children)
  (let ((sets (apply union (map lnode-sets-ref children)))
	(refs (apply union (map lnode-refs-ref children))))
    (make-lnode 'type type
		'refs refs
		'sets sets
		'children children)))

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

(define (sym-addr env sym)
  (in-env? sym (env-symbols env)))

(define (addr->struct env addr)
  (list-ref (list-ref (env-structs env)
		      (first addr))
	    (second addr)))

(define (sym->struct env sym)
  (let ((addr (sym-addr env sym)))
    (if addr
	(addr->struct env addr)
	nil)))

(define (lift exp)
  (lift:exp (macroexpand exp) (make-empty-environment)))

(define (lift:exp exp env)
  (dprintf "lift:exp %a -- %a\n" exp env)
  (cond
   ((symbol? exp)
    (lift:symbol exp env))
   ((atom? exp)
    (make-lnode 'type 'const
		'data exp))
   ;; must have a list, look at its head
   ;; TODO: extract the refs from subnodes
   (else
    (case (first exp)
      (quote
       (make-lnode 'type 'const
		   'data exp))

      (begin
	(new-lnode 'begin (lift:begin (cdr exp) env)))

      (set!
       (let ((node (new-lnode 'set! (lift:exp (third exp) env))))
	 (lnode-sets-set! node
			  (list (lift:symbol (second exp) env)))
	 node))

      (if
       (new-lnode 'if
		  (list (lift:exp (second exp) env)
			(lift:exp (third exp) env)
			(lift:exp (fourth exp) env))))

      (lambda
	  (lift:lambda (second exp)
		       (cddr exp)
		       env))

      (else
       (new-lnode 'apply
		  (cons
		   (lift:exp (first exp) env)
		   (map (lambda (e) (lift:exp e env))
			(cdr exp)))))))))


(define (lift:symbol sym env)
  (dprintf "lift:symbol %a -- %a\n" sym env)
  (let ((var (sym->struct env sym)))
    (if var
	(make-lnode 'type 'lref
		    'refs (list var))
	(make-lnode 'type 'gref
		    'children (list 'global sym)))))

(define (lift:begin exps env)
  (dprintf "lift:begin %a %a\n" exps env)
  (map (lambda (exp) (lift:exp exp env)) exps))

(define (lift:lambda args body env)
  (dprintf "lift:lambda %a, %a\n" args body)
  (let* ((new-env (extend-environment env args))
	 (node (new-lnode 'lambda
			  (lift:begin
			   body
			   new-env))))

    (make-lnode 'type 'lambda
		'refs (difference (lnode-refs-ref node)
				  (first (env-structs new-env)))
		'children (lnode-children-ref node))))



