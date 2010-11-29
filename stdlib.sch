;; very basic define to get us started
(set! define
      (macro (name vars body)
	'(set! ,name (lambda ,vars ,body))))
	
(set! define-syntax
      (macro (name vars body)
	'(set! ,name (macro ,vars ,body))))

(set! nil '())

;; handy primitives
(define map (fn lst)
  (if (null? lst)
      nil
      (cons (fn (car lst)) (map fn (cdr lst)))))

(define not (x)
  (if x
      #f
      #t))

(define cadr (x) (car (cdr x)))
(define first (x) (car x))
(define second (x) (cadr x))

(define-syntax let (bindings body)
  '((lambda ,(map first bindings)
      ,body)
    . ,(map second bindings)))


(define index-of (fn lst)
  (begin
    (define iter (n rest)
      (if (null? rest)
	  nil
	  (if (fn (car rest))
	      n
	      (iter (+ n 1) (cdr rest)))))
    (iter 0 lst)))

(define index-eq (val lst)
  (index-of (lambda (x) (eq? x val)) lst))

(define any? (fn lst)
  (if (null? (index-of fn lst))
      #f
      #t))

(define any-eq? (val lst)
  (if (null? (index-eq val lst))
      #f
      #t))

(define-syntax when (pred conseq)
  (if ,pred
      ,conseq
      nil))

(define-syntax unless (pred conseq)
  (if ,pred
      nil
      ,conseq))

(define nth (lst n)
  (begin
    (define iter (i rest)
      (if (= i n)
	  (car rest)
	  (iter (+ i 1) (cdr rest))))
    (iter 0 lst)))

(define-syntax macroexpand (mac)
  ',mac)

;; now building up function definition with &rest
(define-syntax wrap-rest (vars body)
  (let ((idx (index-eq '&rest vars)))
    (if (null? idx)
	body
	'(let ((,(nth vars (+ 1 idx)) (find-variable '&rest)))
	   ,body))))

(define-syntax define2 (name args body)
  '(set! ,name
	 ,(wrap-rest args
		     (lambda args body))))
			     
		
	    
;(define-syntax lambda2 (name vars body)
;  '(lambda ,vars
     
;; is there a rest?

(set! a '(1 2 3 4))
