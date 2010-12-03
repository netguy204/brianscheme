;; first time a simple loop that only makes ephemeral garbage
'little-ephemeral-garbage
(time (do-times (lambda (x) nil) 1000))

'more-ephemeral-garbage
(time (do-times (lambda (x) (list x x x x)) 1000))

'non-ephemeral-garbage
(define (make-list max-n)
  (let ((result nil))
    (do-times (lambda (x)
		(set! result (cons x result)))
	      max-n)
    (cdr result)))

(set! big-list nil)
(time (set! big-list (make-list 1000)) nil)

'lots-of-math
(time (apply + big-list))
(time (apply * big-list))

'length
(time (length big-list))
