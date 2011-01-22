; Displays a 2D normal distribution using ncurses.
;
; Press q to quit. Try resizing your terminal once it's started!

(require 'random)
(require 'ncurses)

(define (normal-plot)
  "Generate 2D plots of numbers from the normal distribution."
  (with-curses win
    (let ((done #f))
      (while (not done)
        (let* ((w (nc:getmaxx win))
               (half-w (/ w 2.0))
               (h (nc:getmaxy win))
               (half-h (/ h 2.0)))
          (dotimes (i 250)
            (let ((x (+ (* (random:normal) (/ half-w 3.5)) half-w))
                  (y (+ (* (random:normal) (/ half-h 3.5)) half-h)))
              (nc:mvprintw (floor y) (floor x) ".")))
          (if (= (nc:getch) (char->integer #\q))
              (set! done #t)
              (nc:clear)))))))

(define (write-sample file size)
  "Write a sample of the RNG to an output file for ent."
  (let ((func (lambda (port)
                (dotimes (i size)
                  (write-char (integer->char (random 256)) port)))))
    (call-with-output-file file func)))

(require 'unittest)

;; Test the first few numbers of the Mersenne twister.
(define-test (mersenne-test)
  (let ((rng (make <mersenne> 103)))
    (check
     (= 1855903495 (generate rng))
     (= 1112800329 (generate rng))
     (= 42094 (logand *mask-16* (generate rng)))
     (= 1978050084 (generate rng)))))
