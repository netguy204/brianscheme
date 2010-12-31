
(define (abs a)
  (if (< a 0)
      (- 0 a)
      a))

(define (min a b)
  (if (< a b)
      a
      b))

(define (max a b)
  (if (> a b)
      a
      b))

(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (- a (* b (/ a b))))))

(define (make-rat a b)
  (reduce-rat (cons a b)))

(define (numerator a)
  (car a))

(define (denominator a)
  (cdr a))

(define (reduce-rat rat)
  (let ((common (gcd (numerator rat) (denominator rat))))
    (cons (/ (numerator rat) common) (/ (denominator rat) common))))

(define (neg-rat rat)
  (make-rat (- 0 (numerator rat)) (denominator rat)))

(define (add-rat a b)
  (let ((na (numerator a))
	(nb (numerator b))
	(da (denominator a))
	(db (denominator b)))
    (make-rat (+ (* na db) (* nb da)) (* da db))))

(define (sub-rat a b)
  (add-rat a (neg-rat b)))

(define (mul-rat a b)
  (make-rat (* (numerator a) (numerator b))
	    (* (denominator a) (denominator b))))

(define (div-rat a b)
  (make-rat (* (numerator a) (denominator b))
	    (* (denominator a) (numerator b))))

