;; Handy syntactic sugar, using read-macros.

(require 'read)

;; This creates an Arc-like singe-argument lambda function.
;; [+ _ 5] -> (lambda (_) (+ _ 5))

(define-macro-character (#\] port)
  (throw-error "read unexpected ']'" #\]))

(define-macro-character (#\[ port)
  `(lambda (_) ,(read:list port #\])))

;(display (map [+ _ 10] (upto 5)))
