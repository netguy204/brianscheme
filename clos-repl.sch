;; now build up the clos environment and jump into the more
;; object aware repl
(require 'clos)

(define (write-port obj . port)
  (let ((stream (if port
		    (make <native-output-stream>
		      'port (car port))
		    stdout-stream)))
    (print-object stream obj)))

(define (clos-repl)
  (let* ((exp (read-port stdin))
	 (res (eval exp)))
    (unless (eq? res 'quit)
	    (print-object stdout-stream res)
	    (newline)
	    (clos-repl))))

(define exit-hook clos-repl)

(print-object stdout-stream
	      "Evaluate 'quit to exit the advanced REPL\n")

(clos-repl)

(print-object stdout-stream
"You're now in the primitive REPL. Everything works the same here but
the reader and the printer are a bit more primitive.\n")


