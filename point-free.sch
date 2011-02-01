(define (compliment fn)
  "function that returns (not (fn))"
  (lambda (x) (not (fn x))))

(define (compose . funcs)
  "Compose a series of functions into a new single function."
  (let ((rev-funcs (reverse funcs)))
    (letrec ((reduce2 (lambda (last rest)
                        (if (null? rest)
                            last
                            (reduce2 (apply (car rest) (list last))
                                     (cdr rest))))))
      (lambda args
        (reduce2 (apply (car rev-funcs) args) (cdr rev-funcs))))))

(define (curry fn . args)
  (lambda args2
    (apply fn (append args args2))))

(define (rcurry fn . args)
  (lambda args2
    (apply fn (append args2 args))))

(define (always x)
  (lambda args x))

(define (identity x)
  x)
