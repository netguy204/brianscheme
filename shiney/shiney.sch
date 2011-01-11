;; "Something Shiney"
;;
;; An entry into the January 2011 lisp game programming expo
;;
;; Brian Taylor
;;


;(require 'clos)
(require 'ncurses)

(define-struct sprite
  "may have multiple frames"
  ((frames)
   (frame-count)
   (width)
   (height)))

(define-struct game-object
  "fundamental game object"
  ((sprite)
   (width)
   (height)))

(define (get-frame go num)
  (vector-ref (sprite-frames (game-object-sprite go)) num))

(define (image-width image)
  (string-length (vector-ref image 0)))

(define (image-height image)
  (vector-length image))

(define (build-sprite images)
  "build a sprite from a list of images"
  (let ((exp-width (image-width (first images)))
	(exp-height (image-height (first images))))

    (dolist (image images)
      (if (not (= (image-height image)
		  exp-height))
	  (throw-error "not all images had the same height")
	  (dovector (row image)
	    (if (not (= (string-length row))
		     exp-width)
		(throw-error "image had non-rectangular dimensions")))))

    (make-sprite 'frames (apply vector images)
		 'frame-count (length images)
		 'width exp-width
		 'height exp-height)))

(define (build-game-object sprite)
  "build a game object from a sprite"
    (make-game-object 'sprite sprite
		      'width (sprite-width sprite)
		      'height (sprite-height sprite)))

(define table-graphic
  #(" ............ "
    "/------------\\"
    "||          ||"
    "||          ||"))

(define waiter-graphic-1
  #(" O "
    "/| "
    " | "
    " |\\"))

(define waiter-graphic-2
  #(" O "
    " |\\"
    " | "
    " | "))

(define waiter-graphic-3
  #(" O "
    " | "
    " | "
    "/| "))

(define (dump-game-object obj)
  (dovector (frame (sprite-frames (game-object-sprite obj)))
    (dovector (row frame)
      (display row)
      (newline))
    (newline)
    (newline)))

(define table (build-game-object (build-sprite (list table-graphic))))
(define waiter (build-game-object (build-sprite (list waiter-graphic-1
						      waiter-graphic-2
						      waiter-graphic-3))))



(define (draw-frame frame ul-row ul-col)
  "draw frame starting with its upper left corner at the coordinates
specified"
  (let ((row-number 0)
	(width (image-width frame))
	(str (make-string 2)))
    (display frame) (newline)
    (dovector (row frame)
      (display row) (newline)
      (let ((current-row (+ ul-row row-number)))
	(let char-loop ((idx 0))
	  (when (< idx width)
  	    ;; spaces are transparent so don't draw them
	    (when (not (eq? (string-ref row idx) #\space))
	      ;; build a null terminated string from the char
	      ;; for mvprintw
	      (string-set! str 0 (string-ref row idx))
	      (nc:mvprintw current-row
			   (+ ul-col idx)
			   str))
	    (char-loop (+ idx 1)))))
      (inc! row-number))))

(define (test)
  (with-curses win
    (nc:noecho)
    (nc:cbreak)
    (nc:keypad win #t)
    (nc:halfdelay 1)

    (nc:clear)

    (dotimes (n 3)
      (draw-frame (get-frame waiter n) 10 (+ 5 (* 20 n)))
      (draw-frame (get-frame table 0) 10 (+ 10 (* 20 n))))

    (nc:refresh)
    (sleep 5)))

(test)
