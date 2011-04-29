
(define (make-new-names vars)
  (map (lambda (var) (cons var (gensym))) vars))

(define (alpha-convert exp vars)
  (cond
   ((symbol? exp)
    (if-let ((new (assq exp vars)))
	    (cdr new)
	    exp))

   ((atom? exp) exp)
     
   (else 
    (record-case exp
      (if-compiling (then else)
        (list 'if-compiling 
	      (alpha-convert then vars)
	      (alpha-convert else vars)))
            
      (quote (obj) (list 'quote obj))
      
      (begin exps
	(cons 'begin 
	      (map (lambda (exp)
		     (alpha-convert exp vars))
		   exps)))
      
      (set! (sym val)
        (list 'set!
	      (alpha-convert sym vars)
	      (alpha-convert val vars)))
      
      (if (test then . else)
	  (list 'if
		(alpha-convert test vars)
		(alpha-convert then vars)
		(if else
		    (alpha-convert (car else) vars))))
      
      (lambda (largs . body)
	(let ((free-args (filter (lambda (var)
				   (not (member? (car var)
						 largs)))
				 vars)))
	  `(lambda ,largs
	     ,@(map (lambda (e)
		      (alpha-convert e free-args))
		    body))))
      
      (else
       (if (comp-macro? (first exp))
	   (alpha-convert (comp-macroexpand0 exp) args)
	   (map (lambda (e)
		  (alpha-convert e vars))
		exp)))))))

    
(define (make-lambda-args args remap)
  (cond
   ((symbol? args)
    (cdr (assq args remap)))

   ((null? args) nil)
   
   ((pair? args)
    (cons (cdr (assq (car args) remap))
	  (make-lambda-args (cdr args) remap)))))


(define (inline-lambda exp)
  (record (cdr exp)
    (vars . body)
    (let* ((remaped (make-new-names (make-true-list vars)))
	   (alpha-exp (map (lambda (exp)
			     (alpha-convert exp remaped))
			   body)))
      
      `(inlined-lambda ,(make-lambda-args vars remaped)
		       . ,alpha-exp))))

