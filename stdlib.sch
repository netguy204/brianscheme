;; very basic define to get us started
(set! define0
      (macro (name vars body)
	'(set! ,name (lambda ,vars ,body))))
	
(set! define-syntax0
      (macro (name vars body)
	'(set! ,name (macro ,vars ,body))))

(set! nil '())

(set! *debug-stack* nil)

;; primitive let since we don't have &rest yet
(define-syntax0 let0 (bindings body)
  '((lambda ,(map first bindings)
      ,body)
    . ,(map second bindings)))

(define-syntax0 push (obj dst)
  '(set! ,dst (cons ,obj ,dst)))

(define-syntax0 pop (dst)
  '((lambda (top)
     (set! ,dst (cdr ,dst))
     top) (car ,dst)))

(define-syntax0 without-debug (body)
  body)

;(define-syntax0 without-debug (body)
;  '(begin
;     (set! *debug* #f)
;     (push #f *debug-stack*)
;
;     ((lambda (result)
;	(pop *debug-stack*)
;	(if (null? *debug-stack*)
;	    (set! *debug* #t)
;	    nil)
;	result) ,body)))


;; handy primitives
(define0 map (fn lst)
  (without-debug
   (begin
     (define0 iter (rest)
       (if (null? rest)
	   nil
	   (cons (fn (car rest)) (iter (cdr rest)))))
     (iter lst))))

(define0 not (x)
  (if x
      #f
      #t))

(define0 cadr (x) (car (cdr x)))
(define0 first (x) (car x))
(define0 second (x) (cadr x))

(define0 index-of (fn lst)
  (without-debug
   (begin
     (define0 iter (n rest)
       (if (null? rest)
	   nil
	   (if (fn (car rest))
	       n
	       (iter (+ n 1) (cdr rest)))))
     (iter 0 lst))))

(define0 index-eq (val lst)
  (index-of (lambda (x) (eq? x val)) lst))

(define0 nth (lst n)
  (without-debug
   (begin
     (define0 iter (i rest)
       (if (= i n)
	   (car rest)
	   (iter (+ i 1) (cdr rest))))
     (iter 0 lst))))

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
(define-syntax0 define-syntax (name args fbody)
  '(set! ,name
	 ,(let0 ((idx (index-eq '&rest args)))
	    (if (null? idx)
		'(macro ,args ,fbody)
		'(macro ,args
		   (let0 ((,(nth args (+ 1 idx)) (find-variable '&rest)))
			 ,fbody))))))

(define-syntax define (name args &rest fbody)
  '(set! ,name
	 ,(let0 ((idx (index-eq '&rest args)))
	    (if (null? idx)
		'(lambda ,args ,fbody)
		'(lambda ,args
		   (let0 ((,(nth args (+ 1 idx)) (find-variable '&rest)))
			 . ,fbody))))))

(define-syntax let (bindings &rest body)
  '((lambda ,(map first bindings)
      (begin . ,body))
    . ,(map second bindings)))


(set! a '(1 2 3 4))

(macroexpand0 '(define foo (a &rest b) b))
		
;; now we can make a better define to keep going
;(define any? (fn lst)
;  (if (null? (index-of fn lst))
;      #f
;      #t))

;(define any-eq? (val lst)
;  (if (null? (index-eq val lst))
;      #f
;      #t))

;(define-syntax when (pred conseq)
;  '(if ,pred
;       ,conseq
;       nil))

;(define-syntax unless (pred conseq)
;  '(if ,pred
;       nil
;       ,conseq))


	    
;(define-syntax lambda2 (name vars body)
;  '(lambda ,vars
     


