;; wouldn't it be super if there were persistant data structures to go
;; along with these nifty generic interfaces?

(define-generic gfirst
  "generic first item in a list")

(define-generic grest
  "generic rest item in a list")

(define-generic conj
  "generic appender")

(define-generic glength
  "generic length calculator")

;; functions that build on the generic interfaces
(define (into base values)
  (let loop ((result base)
	     (remaining values))
    (if remaining
	(loop (conj result (gfirst remaining))
	      (grest remaining))
	result)))

;; dealing with lists
(define-method (gfirst (l <pair>))
  (car l))

(define-method (grest (l <pair>))
  (cdr l))

(define-method (conj (l <null>) other)
  (cons other l))

(define-method (conj (l <pair>) other)
  (cons other l))

(define-method (glength (l <pair>))
  (length l))


;; dealing with vectors
(define-method (gfirst (v <vector>))
  (vector-ref v 0))

(define-method (grest (v <vector>))
  (if (or (= (vector-length v) 0)
	  (= (vector-length v) 1))
      nil
      (apply vector (grest (vector->list v)))))

(define-method (conj (v <vector>) other)
  (let ((new (make-vector (+ (vector-length v) 1))))
    (dotimes (idx (vector-length v))
      (vector-set! new idx (vector-ref v idx)))
    (vector-set! new (vector-length v) other)
    new))

(define-method (glength (l <vector>))
  (vector-length l))

