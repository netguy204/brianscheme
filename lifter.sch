(define (dprintf . args)
  "debug printf"
  (apply printf args))

(define (dprintf . args) nil)

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
   new-bindings
   children))

(define-struct lvar
  "represents a variable"
  (name
   oname
   properties))

(define (new-lnode type children)
  "create a new node, pulling up the refs and sets from the children"
  (let ((sets (apply union (map lnode-sets-ref children)))
	(refs (apply union (map lnode-refs-ref children))))
    (make-lnode 'type type
		'refs refs
		'sets sets
		'children children)))

(define (make-environment syms structs)
  "create a new environment"
  (cons syms structs))

(define (make-empty-environment)
  "create an empty environment"
  (make-environment nil nil))

(define (env-symbols env)
  "get the traditional symbol representation"
  (car env))

(define (env-structs env)
  "get the parallel struct representation"
  (cdr env))

(define (new-lvar old-name)
  "create a new bound variable representing a given user-provided
name"
  (make-lvar
   'name (gensym)
   'oname old-name
   'properties (make-hashtab-eq 10)))

(define (extend-environment env vars)
  "add a frame (structs and symbols) to the enviornment"
  (let ((tvars (make-true-list vars)))
    (make-environment
      (cons tvars (env-symbols env))
      (cons (map new-lvar tvars) (env-structs env)))))

(define (sym-addr env sym)
  "compute the address of a given user symbol in the current
environment"
  (in-env? sym (env-symbols env)))

(define (addr->struct env addr)
  "look up the structure for a given address"
  (list-ref (list-ref (env-structs env)
		      (first addr))
	    (second addr)))

(define (sym->struct env sym)
  "return the structure that now represents a particular symbol in a
given environment"
  (let ((addr (sym-addr env sym)))
    (if addr
	(addr->struct env addr)
	nil)))

(define (lift exp)
  "perform a lift (actually an alpha conversion ATM)"
  (lift:exp (macroexpand exp) (make-empty-environment)))

(define (lift:exp exp env)
  "lift an expression"
  (dprintf "lift:exp %a -- %a\n" exp env)
  (cond
   ((symbol? exp)
    (lift:symbol exp env))
   ((atom? exp)
    (make-lnode 'type 'const
		'children exp))
   ;; must have a list, look at its head
   ;; TODO: extract the refs from subnodes
   (else
    (case (first exp)
      (quote
       (make-lnode 'type 'const
		   'children exp))

      (begin
	(new-lnode 'begin (lift:begin (cdr exp) env)))

      (set! (lift:set exp env))

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
  "lift a symbol"
  (dprintf "lift:symbol %a -- %a\n" sym env)
  (let ((var (sym->struct env sym)))
    (if var
	(make-lnode 'type 'lref
		    'refs (list var))
	(make-lnode 'type 'gref
		    'children (list 'global sym)))))

(define (lift:set exp env)
  "lift a set! directive"
  (let ((node (new-lnode 'set!
			 (list (lift:exp (third exp) env))))
	(var (sym->struct env (second exp))))
    (when var (lnode-sets-set! node (list var)))
    node))

(define (lift:begin exps env)
  "lift a begin directive"
  (dprintf "lift:begin %a %a\n" exps env)
  (map (lambda (exp) (lift:exp exp env)) exps))

(define (lift:lambda args body env)
  "lift a lambda directive"
  (dprintf "lift:lambda %a, %a\n" args body)
  (let* ((new-env (extend-environment env args))
	 (node (new-lnode 'lambda
			  (lift:begin
			   body
			   new-env))))

    (make-lnode 'type 'lambda
		'refs (difference (lnode-refs-ref node)
				  (first (env-structs new-env)))
		'sets (difference (lnode-sets-ref node)
				  (first (env-structs new-env)))
		'new-bindings (first (env-structs new-env))
		'children (lnode-children-ref node))))



(define (lift:pp node)
  "pretty-print the output of lift"
  (cond
   ((not (lnode? node)) node)
   (else
    (case (lnode-type-ref node)
      (const (list 'quote (lnode-children-ref node)))
      (begin `(begin . ,(map lift:pp (lnode-children-ref node))))
      (set! `(set! ,(lvar-name-ref (first (lnode-sets-ref node)))
		   ,(lift:pp (first (lnode-children-ref node)))))
      (if `(if ,(lift:pp (first (lnode-children-ref node)))
	       ,(lift:pp (second (lnode-children-ref node)))
	       ,(lift:pp (third (lnode-children-ref node)))))
      (lambda `(lambda
		 (new-bindings
		 ,(map lvar-name-ref (lnode-new-bindings-ref node)))
		 (free-read-bindings
		 ,(map lvar-name-ref (lnode-refs-ref node)))
		 (free-write-bindings
		 ,(map lvar-name-ref (lnode-sets-ref node)))
		 . ,(map lift:pp (lnode-children-ref node))))
      (apply (cons
	      (lift:pp (first (lnode-children-ref node)))
	      (map lift:pp (rest (lnode-children-ref node)))))
      (lref (lvar-name-ref (first (lnode-refs-ref node))))
      (gref (lnode-children-ref node))
      (else (error "don't know how to process" (lnode-type-ref node)))))))

