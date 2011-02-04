;; Syntactic sugar examples, using read-macros.

(require 'read)

;; This is the classic example that produces a range of characters
;; using brackets. #[2 7] => (2 3 4 5 6 7)

(define-macro-character (#\] port)
  (throw-error "read unexpected ']'" #\]))

(define-dispatch-macro-character (#\# #\[ port)
  (display "macro\n")
  (let* ((range (read:list port #\]))
	 (min (first range))
	 (max (second range))
	 (lst '()))
    (dotimes (i (- max min -1))
      (push! (- max i) lst))
    (list 'quote lst)))

;(display #[2 7])

;; This creates an Arc-like singe-argument lambda function.
;; [+ _ 5] -> (lambda (_) (+ _ 5))
;; This relies on the #\] macro defined above.

(define-macro-character (#\[ port)
  `(lambda (_) ,(read:list port #\])))

;(display (map [+ _ 10] (upto 5)))
