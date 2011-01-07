(require 'pth)

(define (hello)
  (display "hello\n")
  (pth:sleep 1)
  (hello))

(define (bye)
  (display "bye\n")
  (pth:usleep (* 1000 768))
  (bye))

(pth:spawn hello)
(bye)
