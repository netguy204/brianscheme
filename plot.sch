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
	    (ffi:funcall popen 'ffi-pointer *gnuplot-program* "w")))

    (define (plot:raw . strings)
      "Send bare strings to gnuplot without flushing."
      (ffi:funcall fprintf 'ffi-uint
                   *gnuplot-handle* "%s" (apply string-append strings)))

    (define (plot:command command)
      "Send a command to gnuplot."
      (ffi:funcall fprintf 'ffi-uint *gnuplot-handle* "%s\n" command)
      (ffi:funcall fflush 'ffi-uint *gnuplot-handle*)
      #t)

    (define (plot:send-vectors . vecs)
      "Send vectors as columns to gunplot."
      (dotimes (i (vector-length (car vecs)))
        (for-each (lambda (vec)
                    (plot:raw (number->string (vector-ref vec i)) " ")) vecs)
        (plot:raw "\n"))
      (plot:command "e"))

    (define (plot:send-list lst)
      "Send a list to gunplot."
      (for-each (compose plot:command number->string) lst)
      (plot:command "e"))

    (define (plot:vector vec)
      "Plot a vector."
      (plot:command "set style data lines")
      (plot:command "plot '-'")
      (plot:send-vectors vec))

    (define (plot:list lst)
      "Plot a list."
      (plot:command "set style data lines")
      (plot:command "plot '-'")
      (plot:send-list lst))

    (define (plot:hist lst . num-bins)
      "Plot histogram of data."
      (let* ((num-bins (if num-bins (car num-bins) 10))
             (bins (make-vector num-bins 0))
             (min (* 1.0 (apply min lst)))
             (max (* 1.0 (apply max lst)))
             (range (- max min))
             (xs (make-vector num-bins 0)))
        ;; Generate x-data
        (dotimes (i num-bins)
          (vector-set! xs i (+ min (/ (* i range) num-bins))))
        ;; Generate y-data
        (dolist (x lst)
          (let ((i (floor (* (/ (- x min) range) (- num-bins 1)))))
            (vector-set! bins i (+ 1 (vector-ref bins i)))))
        (plot:command "set boxwidth 1 relative")
        (plot:command "set style fill solid 1.0 border -1")
        (plot:command "plot '-' with boxes lc rgb \"blue\"")
        (plot:send-vectors xs bins)
        bins))))

(plot:init)
