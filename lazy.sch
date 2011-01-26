;; Implement the classic delay/force combo directly by representing a
;; delay as the cons of its value (nil of not yet forced) and the
;; closure that computes it (nil if it has been forced)
(define-syntax (delay . body)
  "create a computation that can be completed later"
  `(cons nil (lambda () . ,body)))

(define (force fn)
  "compute and or return the value of a delay"
  (when (and (not (null? (cdr fn))) (null? (car fn)))
	(set-car! fn ((cdr fn)))
	(set-cdr! fn nil))
  (car fn))
