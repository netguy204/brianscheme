;; now build up the clos environment and jump into the more
;; object aware repl
(require 'clos)

(define (write-port obj . port)
  (let ((stream (if port
		    (make <native-output-stream>
		      'port (car port))
		    stdout-stream)))
    (print-object stream obj)))

;; define a VM exception handler that uses our higher level condition
;; system
(set-error-restart!
 (lambda (ex)
   (raise (cons 'vm-error ex))))

(define (clos-repl)
    (let* ((exp (with-exception-handler
		 (lambda (ex)
		   (write-stream stderr-stream "clos-repl, reader-error: ")
		   (print-object stderr-stream ex)
		   (newline)
		   (clos-repl))

		 (lambda () (read-port stdin))))

	   (res (with-exception-handler
		 (lambda (ex)
		   (write-stream stderr-stream "clos-repl: ")
		   (print-object stderr-stream ex)
		   (newline)
		   (clos-repl))

		 (lambda () (eval exp)))))

      (when (eof-object? res)
	    (newline)
	    (exit 0))

      (print-object stdout-stream res)
      (newline)
      (clos-repl)))

