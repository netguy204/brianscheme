(define (zip2 l1 l2)
  (let ((result nil))
    (let loop ((r1 l1)
	       (r2 l2))
      (if (and r1 r2)
	  (begin
	    (push! (cons (car r1)
			 (car r2))
		   result)
	    (loop (cdr r1)
		  (cdr r2)))
	  (reverse result)))))

(define (make-new-names vars)
  (map (lambda (var) (cons var (gensym))) vars))

(define (make-notepad)
  (cons nil nil))

(define (get-notes notepad)
  (car notepad))

(define (push-note! notepad note)
  (set-car! notepad (cons note (get-notes notepad))))

(define (remap-if-defined sym remapped)
  (if-let ((new (assq sym remapped)))
	  (cdr new)
	  sym))

(define (find-inlined-vars exp found)
  (cond
   ((atom? exp)
    found)
   (else
    (record-case exp
      (if-compiling (then else)
	 (find-inlined-vars then (find-inlined-vars else)))
      (quote (obj) found)
      (begin exps
        (reduce (lambda (found exp)
		  (find-inlined-vars exp found))
		exps
		found))
      (set! (sym val)
	    (find-inlined-vars val found))
      (if (test then . else)
	  (find-inlined-vars test
	    (find-inlined-vars then
	       (if else (find-inlined-vars (car else) found)))))
      (lambda (args . body)
	(reduce (lambda (found exp)
		  (find-inlined-vars exp found))
		body
		found))
      (inlined-lambda (args body)
	;; inlinings always bubble up so no need to search body
	(append args found))
      
      (else
       (reduce (lambda (found exp)
		 (find-inlined-vars exp found))
	       exp
	       found))))))

(define (alpha-convert exp vars inline-notes)
  (cond
   ((symbol? exp)
    (remap-if-defined exp vars))

   ((atom? exp) exp)
     
   (else 
    (record-case exp
      (if-compiling (then else)
        (list 'if-compiling 
	      (alpha-convert then vars inline-notes)
	      (alpha-convert else vars inline-notes)))
            
      (quote (obj) (list 'quote obj))
      
      (begin exps
	(cons 'begin 
	      (map (lambda (exp)
		     (alpha-convert exp vars inline-notes))
		   exps)))
      
      (set! (sym val)
        (list 'set!
	      (alpha-convert sym vars inline-notes)
	      (alpha-convert val vars inline-notes)))
      
      (if (test then . else)
	  (list 'if
		(alpha-convert test vars inline-notes)
		(alpha-convert then vars inline-notes)
		(if else
		    (alpha-convert (car else) vars inline-notes))))
      
      (lambda (largs . body)
	(let ((free-args (filter (lambda (var)
				   (not (member? (car var)
						 largs)))
				 vars)))
	  `(lambda ,largs
	     ,@(map (lambda (e)
		      (alpha-convert e free-args inline-notes))
		    body))))
      
      (else
       (if (comp-macro? (first exp))
	   (alpha-convert (comp-macroexpand0 exp) vars inline-notes)

	   (if (and (pair? (first exp))
		    (eq? 'lambda (first (first exp))))
	       ;; head is lambda and thus inline-able
	       (record (rest (first exp))
	         (args . body)
		 (let ((remapped (make-new-names (make-true-list args)))
		       (parms (rest exp)))
		   
		   (if inline-notes
		       (begin
			 (push-note! inline-notes remapped)
			 (generate-inlined args (append remapped vars) parms body inline-notes))
		       ;; we're starting an inline block, make a notepad
		       (let ((notepad (make-notepad)))
			 (push-note! notepad remapped)
			 (let ((inlined (generate-inlined args (append remapped vars) parms body notepad)))
			   `(inlined-lambda ,(append-all (get-notes notepad))
					    ,inlined))))))
	       ;; build up non-inlined call
	       (map (lambda (e)
		      (alpha-convert e vars inline-notes))
		    exp))))))))

(define (generate-sets args parms remapped inline-notes)
  (cond
   ((symbol? args)
    ;; this is a dotted list
    (list
     `(set! ,(remap-if-defined args remapped)
	    (list . ,(map (lambda (parm)
			    ;; is it bad that this has notes from an outside scope?
			    (alpha-convert parm remapped inline-notes))
			  parms)))))
   ((pair? args)
    (cons
     `(set! ,(remap-if-defined (car args) remapped)
	    ,(alpha-convert (car parms) remapped inline-notes))
     (generate-sets (cdr args) (cdr parms) remapped inline-notes)))
   
   ((null? args) nil)))


(define (generate-inlined args remapped parms body notepad)
  `(begin
     ;; generate the sets
     ,@(generate-sets args parms remapped notepad)
     
     ;; inline the body
     ,@(map (lambda (form)
	      (alpha-convert form remapped notepad))
	    body)))

(define (make-lambda-args args remap)
  (cond
   ((symbol? args)
    (cdr (assq args remap)))

   ((null? args) nil)
   
   ((pair? args)
    (cons (cdr (assq (car args) remap))
	  (make-lambda-args (cdr args) remap)))))

;; TODO: define a scoped-inlined-lambdas that the compiler can use to
;; find the inlined-lambdas that it needs to allocate stack space for.
;;
(define (promote-scoped-inlined-lambda exp)
  (cond
   ((atom? exp) nil)
   (record-case exp
     )))

;; is there any way to stop duplicating the language?

;; Make sure that inlined solutions bubble up
;;
;; If inlinable encountered and we are inlining then inline fully and
;; push the new vars onto the inline stack
;;
;; If inlinable encountered and we're not inlining then generate the
;; inlined-lambda code and set the inlining flag
(define (inline-lambda exp)
  (record (cdr exp)
    (vars . body)
    (let* ((remaped (make-new-names (make-true-list vars)))
	   (alpha-exp (map (lambda (exp)
			     (alpha-convert exp remaped))
			   body)))
      
      `(inlined-lambda ,(make-lambda-args vars remaped)
		       . ,alpha-exp))))

