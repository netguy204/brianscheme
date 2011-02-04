;; Handy syntactic sugar, using read-macros.

(require 'read)

;; This creates an Arc-like singe-argument lambda function.
;; [+ _ 5] -> (lambda (_) (+ _ 5))
;; This relies on the #\] macro defined above.

(define-macro-character (#\[ port)
  `(lambda (_) ,(read:list port #\])))

;(display (map [+ _ 10] (upto 5)))
