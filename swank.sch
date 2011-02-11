;;; swank.sch --- BS swank server

;; DESCRIPTION: Provides Swank services for use with Emacs and Slime.
;;
;; Connecting and basic use works, evaluating code from a buffer or
;; the REPL. Autodoc requests are ignored. Debugging doesn't work at
;; all yet.

(require 'socket)

(define *swank-port* 4005)

(define (swank-listen . port-arg)
  "Start the swank server."
  (let* ((port0 (car-else port-arg *swank-port*))
	 (port (if (zero? port0) (+ 1024 (random 10000)) port0)))
    (printf "Starting swank server on port %a\n" port)
    (define *swank-stream* (make-server-stream port))
    (while #t
	   (let ((exp (swank:recv)))
	     (print-object stdout-stream exp)
	     (newline)
	     (let ((out
		    `(:return ,(apply (eval (car (second exp)))
				      (cdr (second exp)))

			      ,(fifth exp))))
	       (print-object stdout-stream out)
	       (newline)
	       (swank:send out))))))

(define (swank:send exp)
  "Send the expression to SLIME."
  (let ((buffer (make-string-buffer))
	(pad (lambda (str)
               (if (< (string-length str) 6)
                   (string-append (make-string (- 6 (string-length str)) #\0)
                                  str)
                   str))))
    (print-object buffer exp)
    (write-stream buffer "\n")
    (let ((str (string-buffer->string buffer)))
      (write-stream *swank-stream*
		    (pad (integer->string (string-length str) 'base 16)))
      (write-stream *swank-stream* str))))

(define (swank:recv)
  "Read an expression from SLIME."
  (dotimes (i 6)
    (read-stream-char *swank-stream*)) ; throw out the number
  (let ((exp (read-stream *swank-stream*)))
    (read-stream-char *swank-stream*) ; throw out newline
    exp))

;; Communication functions.
(define (object->string form)
  "stringify a form using the standard printer"
  (string-buffer->string
   (doto (make-string-buffer)
     (print-object form))))

(define (safe-eval exp)
  "for internal evaluations: returns exception or result"
  (guard
   (ex
    (#t ex))

   (eval exp)))

(define-syntax (with-standard-return . body)
  `(cons ':ok (begin . ,body)))

(define (swank:connection-info . args)
  (with-standard-return
   (list `(:pid ,(getpid) :style nil :lisp-implementation (:name "bsch")
		:version "2010-12-10"))))

(define (swank:swank-require . args)
  "Ignore for now."
  (with-standard-return
   '(("SWANK"))))

(define (swank:create-repl . args)
  "Ignore for now."
  (with-standard-return
   '(("BSCH" "USER"))))

(define (swank:interactive-eval . args)
  "Evaluate expression from SLIME."
  (with-standard-return
   (list (object->string (safe-eval (read-from-string (car args)))))))

(define (swank:listener-eval . args)
  "Evaluate code from SLIME REPL."
  (with-standard-return
   (swank:send `(:presentation-start 1 :repl-result))
   (swank:send `(:write-string ,(object->string
				 (safe-eval (read-from-string (car args))))
			       :repl-result))
   (swank:send `(:presentation-end 1 :repl-result))
   (swank:send `(:write-string "\n" :repl-result))
   `(nil)))

(define (swank:compile-string-for-emacs . args)
  "handle ctrl+c ctrl+c method of sending forms"
  (list ':ok
	`(:compilation-result
	  nil
	  ,(object->string (safe-eval (read-from-string (car args)))))))

(define (swank:autodoc . args)
  "Ignore for now."
  (with-standard-return
   (list "")))

(define (swank:buffer-first-change . args)
  (with-standard-return
   (list "")))

;; un-comment to have swank load on (require 'swank):

;(swank-listen)

;; or build an image by evaluating in a repl:

;; (require 'swank)
;; (save-image "swank" 'executable #t 'toplevel swank-listen 'compress #t)



