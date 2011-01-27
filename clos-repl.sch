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
    (if (eof-object? res)
        (exit 0))
    (print-object stdout-stream res)
    (newline)
    (clos-repl)))

(define exit-hook clos-repl)

(clos-repl)




