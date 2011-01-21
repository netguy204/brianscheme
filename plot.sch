(require 'ffi)

(define *gnuplot-program* "gnuplot")
(define *gnuplot-handle* #f)

(with-library (handle nil)
  (let ((popen (ffi:dlsym handle "popen"))
	(fprintf (ffi:dlsym handle "fprintf"))
	(fflush (ffi:dlsym handle "fflush"))
	(pclose (ffi:dlsym handle "pclose")))

    (define (plot:init)
      "Initialize the plotting library."
      (set! *gnuplot-handle*
	    (ffi:funcall popen 'ffi-pointer *gnuplot-program* "w"))
      (plot:command "set style data lines"))

    (define (plot:command command)
      "Send a command to gnuplot."
      (ffi:funcall fprintf 'ffi-uint *gnuplot-handle* "%s\n" command)
      (ffi:funcall fflush 'ffi-uint *gnuplot-handle*)
      #t)

    (define (plot:send-vector vec)
      "Send a vector to gunplot."
      (dotimes (i (vector-length vec))
	(plot:command (number->string (vector-ref vec i))))
      (plot:command "e"))

    (define (plot:send-list lst)
      "Send a list to gunplot."
      (for-each (lambda (x) (plot:command (number->string x))) lst)
      (plot:command "e"))

    (define (plot:vector vec)
      "Plot a vector."
      (plot:command "plot '-'")
      (plot:send-vector vec))

    (define (plot:list lst)
      "Plot a list."
      (plot:command "plot '-'")
      (plot:send-list lst))))

(plot:init)
