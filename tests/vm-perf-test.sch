(require 'compiler)

(define (write-bytecode fn file)
  (let ((out (open-output-port file)))
    (write-port out fn)
    (close-output-port out)))

(define (assert pred message)
  (if (not pred)
      (throw-error message)
      #t))

(define-syntax (vm-test expr)
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
(set! map2 ((compiler (compile-together 'map 'mapr
					'reverse 'cons 'car
					'cdr 'null? 'nil))))

(define (make-list max-n)
  (let ((result nil))
    (do-times (lambda (x)
		(set! result (cons x result)))
	      max-n)
    result))

'make-big-list
(set! big-list nil)
(time (set! big-list (make-list 100000)) nil)

'big-map
(set! m1 nil)
(time (map (lambda (x) (+ 1 x)) big-list) 'done)

'compiled-big-map
(set! m2 nil)
(time (map2 f big-list) 'done)

'hybrid-big-map
(time (map2 (lambda (x) (+ 1 x)) big-list) 'done)

'mark-and-sweep
(mark-and-sweep)

(dump-compiled-fn map2)

(dotimes (i 10)
	 (time (map2 f big-list)))

(exit 0)

