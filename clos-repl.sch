;; now build up the clos environment and jump into the more
;; object aware repl
(require 'clos)

(define (clos-repl)
  (let* ((exp (read-port stdin))
	 (res (eval exp)))
    (unless (eq? res 'quit)
	    (print-object stdout-stream res)
	    (newline)
	    (clos-repl))))

(print-object stdout-stream
	      "Evaluate 'quit to exit the advanced REPL\n")

(clos-repl)

(print-object stdout-stream
"You're now in the primitive REPL. Everything works the same here but
the reader and the printer are a bit more primitive.\n")


