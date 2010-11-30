;; very basic define to get us started
(set! *debug* #f)

(set! define0
      (macro (name vars body)
	'(set! ,name (lambda ,vars ,body))))
	
(set! define-syntax0
      (macro (name vars body)
	'(set! ,name (macro ,vars ,body))))

(set! nil '())

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
	  (cons (fn (car rest)) (iter (cdr rest)))))
    (iter lst)))

(define0 not (x)
  (if x
      #f
      #t))

(define0 cadr (x) (car (cdr x)))
(define0 first (x) (car x))
(define0 second (x) (cadr x))

(define0 index-of (fn lst)
  (begin
    (define0 iter (n rest)
      (if (null? rest)
	  nil
	  (if (fn (car rest))
	      n
	      (iter (+ n 1) (cdr rest)))))
    (iter 0 lst)))

(define0 nth (lst n)
  (begin
    (define0 iter (i rest)
      (if (= i n)
	  (car rest)
	  (iter (+ i 1) (cdr rest))))
    (iter 0 lst)))

(define0 index-eq (val lst)
  (index-of (lambda (x) (eq? x val)) lst))

(define0 length=1 (lst)
  (if (not (null? (car lst)))
      (if (null? (cdr lst))
	  #t
	  #f)
      #f))
      
;; now building up function definition with &rest
(define-syntax0 wrap-rest (type args fbody)
  (let0 ((idx (index-eq '&rest args)))
	(if (null? idx)
	    '(,type ,args ,fbody)
	    '(,type ,args
		    (let0 ((,(nth args (+ 1 idx)) (find-variable '&rest)))
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


(define-syntax when (pred conseq)
  '(if ,pred
       ,conseq
       nil))

(define-syntax unless (pred conseq)
  '(if ,pred
       nil
       ,conseq))

(define-syntax cond (&rest clauses)
  (if (null? clauses)
      #f
      '(if ,(first (car clauses))
	   ,(second (car clauses))
	   (cond . ,(cdr clauses)))))

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
  (if (symbol? name)
      '(set! ,name . ,body)
      '(define ,(first name)
	 (wrap-rest lambda ,(cdr name) (begin . ,body)))))

;; now that we have a proper define/-syntax we can keep going
(define (any? fn lst)
  (if (null? (index-of fn lst))
      #f
      #t))

(define (any-eq? val lst)
  (if (null? (index-eq val lst))
      #f
      #t))

(define (length items)
  (define (iter a count)
    (if (null? a)
	count
	(iter (cdr a) (+ 1 count))))
  (iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

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

(macroexpand0 '(or #t #t))

'stdlib-loaded
