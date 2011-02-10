;;; swank.sch --- BS swank server

;; DESCRIPTION: This library will eventually provide Swank services
;; for use with Emacs and Slime.

(require 'socket)

(define *swank-port* 4006)

(define (swank-listen . port-arg)
  "Start the swank server."
  (let* ((port0 (car-else port-arg *swank-port*))
	 (port (if (zero? port0) (+ 1024 (random 10000)) port0)))
    (display "Starting swank server on port ")
    (display port)
    (newline)
    (define *swank-stream* (make-server-stream port))
    (while #t
	   (print-object *swank-stream* (eval (read-stream *swank-stream*)))
	   (write-stream *swank-stream* "\n"))))

(swank-listen 0)
