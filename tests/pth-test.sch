(require 'pth)

(define (hello)
  (display "hello\n")
  (pth:sleep 1)
  (hello))

(define (bye)
  (display "bye\n")
  (pth:sleep 2)
  (bye))

(pth:spawn hello)
(bye)
