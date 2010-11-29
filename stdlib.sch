;; very basic define to get us started
(set! define
      (macro (name vars body)
	'(set! ,name (lambda ,vars ,body))))
	
(set! define-syntax
      (macro (name vars body)
	'(set! ,name (macro ,vars ,body))))

;; handy primitives
(define map (fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst)) (mapcar fn (cdr lst)))))

(define not (x)
  (if x
      #f
      #t))

(define any? (fn lst)
  (if (null? lst)
      #f
      (if (fn (car lst))
	  #t
	  (any? fn (cdr lst)))))

;; now building up function definition with &rest
;(define-syntax lambda2 (name vars body)
;  '(lambda ,vars
     
;; is there a rest?
  
