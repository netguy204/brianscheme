; Displays a 2D normal distribution using ncurses.
;
; Press q to quit. Try resizing your terminal once it's started!

(require 'random)
(require 'ncurses)

(define (normal-plot)
  "Generate 2D plots of numbers from the normal distribution."
  (with-curses win
    (let ((done #f)
          (syms ".-+%#"))
      (while (not done)
        (let* ((w (nc:getmaxx win))
               (half-w (/ w 2.0))
               (h (nc:getmaxy win))
               (half-h (/ h 2.0))
               (grid '()))
          (dotimes (i 1500)
            (let* ((x (floor (+ (* (random:normal) (/ half-w 3.5)) half-w)))
                   (y (floor (+ (* (random:normal) (/ half-h 3.5)) half-h)))
                   (key (list x y))
                   (prev (assoc key grid))
                   (val (min 5 (if prev (+ 1 (cdr prev)) 1))))
              (if (not prev)
                  (set! grid (cons (cons key 0) grid)))
              (assoc-set! grid key val)
              (nc:mvprintw y x (substring syms (- val 1) val))))
          (if (= (nc:getch) (char->integer #\q))
              (set! done #t)
              (nc:clear)))))))

(define (calc-pi)
  "Calculate pi by the Monte Carlo method."
  (with-curses win
    (let ((total 0)
          (in 0)
          (step 500))
      (while #t
        (set! step (+ 400 (random 200)))
        (set! total (+ total step))
        (dotimes (i step)
           (let ((x (- (random 2.0) 1.0))
                 (y (- (random 2.0) 1.0)))
             (if (> 1 (+ (* x x) (* y y)))
                 (inc! in))))
        (nc:mvprintw 0 0 (string-append "in    : " (number->string in)))
        (nc:mvprintw 1 0 (string-append "total : " (number->string total)))
        (nc:mvprintw 2 0 (string-append "pi    : "
                                        (number->string (/ (* 4.0 in) total))))
        (nc:refresh)))))

(define (write-byte-sample file size)
  "Write a sample of the RNG to an output file for ent."
  (let ((func (lambda (port)
                (dotimes (i size)
                  (write-char (integer->char (random 256)) port)))))
    (call-with-output-file file func)))

(define (write-text-sample file fn size)
  "Write a sample of the RNG to an output file for ent."
  (let ((func (lambda (port)
                (dotimes (i size)
                  (write-port (fn) port)
                  (write-char (integer->char 10) port)))))
    (call-with-output-file file func)))

(require 'plot)

(define (plot-pdf fn)
  "Plot the PDF of the given RNG function using the plot library."
  (plot:hist (map fn (duplicate *random-state* 10000)) 50))

(require 'unittest)

;; Test the first few numbers of the Mersenne twister.
(define-test (mersenne-test)
  (let ((rng (make <mersenne> 103)))
    (check
     (= 1855903495 (generate rng))
     (= 1112800329 (generate rng))
     (= 42094 (logand *mask-16* (generate rng)))
     (= 1978050084 (generate rng)))))
