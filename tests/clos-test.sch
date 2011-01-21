(require 'unittest)
(require 'clos)

(define-class <test-base> ()
  "base class for test heirarchy")

(define-class <a> (<test-base>)
  "a class")

(define-class <b> (<test-base>)
  "b class")

(define-generic test-method1
  "do a simple single dispatch")

(define-method (test-method1 (obj <a>) other)
  'just-a)

(define-method (test-method1 (obj <b>) other)
  'just-b)

(define *a* (make <a>))
(define *b* (make <b>))

(define-test (clos-test)
  (check
   (eq? 'just-a (test-method1 *a* *b*))
   (eq? 'just-a (test-method1 *a* 1))
   (eq? 'just-b (test-method1 *b* *a*))))

