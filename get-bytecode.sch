(load "compiler.sch")

(define (write-bytecode fn file)
  (let ((out (open-output-port file)))
    (write-port out fn)
    (close-output-port out)))

;(set! fn (compiler '(begin (+ 1 2))))
(set! fn (compiler '((lambda (x) (cons 1 x)) 5)))

(write-bytecode fn "bytecode.sch")

(exit 0)
