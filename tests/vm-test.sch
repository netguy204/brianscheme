(load "compiler.sch")

(define (write-bytecode fn file)
  (let ((out (open-output-port file)))
    (write-port out fn)
    (close-output-port out)))

(define (assert pred message)
  (if (not pred)
      (throw-error message)
      #t))

(define-syntax vm-test (expr)
  `(let ((exp ',expr)
	 (comp nil)
	 (evres nil)
	 (vmres nil))

     (write 'expression exp)
     (set! comp (compiler exp))

     (write 'bytecode (compiled-bytecode comp))
     (set! evres (eval exp))

     (write 'eval-result evres)
     (set! vmres (vm-execute comp))

     (write 'vm-result vmres)
     (assert (equal? evres vmres))
     
     comp))

(set! f ((compiler '(lambda (x) (+ x 1)))))
(set! mapr2 ((compiler (compound->lambda mapr))))

(define (make-list max-n)
  (let ((result nil))
    (do-times (lambda (x)
		(set! result (cons x result)))
	      max-n)
    result))

'make-big-list
(set! big-list nil)
(time (set! big-list (make-list 10000)) nil)

'big-map
(set! m1 nil)
(time (set! m1 (mapr (lambda (x) (+ 1 x)) big-list)) 'done)

'compiled-big-map
(set! m2 nil)
(time (set! m2 (mapr2 big-list f)) 'done)

(assert (equal? m1 m2))

