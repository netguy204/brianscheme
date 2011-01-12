;; "Something Shiney"
;;
;; An entry into the January 2011 lisp game programming expo
;;
;; Brian Taylor
;;


(require 'clos)
(require 'ncurses)

(define-class <sprite> ()
  "a fixed size drawable thing that may have multiple frames"
  ('frames
   'frame-count
   'width
   'height
   'current-frame))

(define-class <game-object> ()
  "fundamental game object"
  ('sprite
   'ul-row
   'ul-col
   'z))

(define-generic current-frame
  "get the current frame for an object")

(define-method (current-frame (sprite <sprite>))
  (let ((frame-num (slot-ref sprite 'current-frame))
	(frames (slot-ref sprite 'frames)))
    (vector-ref frames frame-num)))

(define-method (current-frame (gobj <game-object>))
  (current-frame (slot-ref gobj 'sprite)))

(define-generic width
  "find the width of a displayable thing")

(define-method (width (image <vector>))
  (string-length (vector-ref image 0)))

(define-method (width (sprite <sprite>))
  (width (vector-ref
	  (slot-ref sprite 'frames) 0)))

(define-method (width (gobj <game-object>))
  (width (slot-ref gobj 'sprite)))

(define-generic height
  "find the height of a displayable thing")

(define-method (height (image <vector>))
  (vector-length image))

(define-method (height (sprite <sprite>))
  (height (vector-ref
	   (slot-ref sprite 'frames) 0)))

(define-method (height (gobj <game-object>))
  (height (slot-ref gobj 'sprite)))

(define (build-sprite . images)
  "build a sprite from a list of images"
  (let ((exp-width (width (first images)))
	(exp-height (height (first images))))

    ;; verify that the sprite dimensions are square
    (dolist (image images)
      (if (not (= (height image)
		  exp-height))
	  (throw-error "not all images had the same height")
	  (dovector (row image)
	    (if (not (= (string-length row))
		     exp-width)
		(throw-error "image had non-rectangular dimensions")))))

    (make <sprite>
      'frames (apply vector images)
      'frame-count (length images)
      'width exp-width
      'height exp-height
      'current-frame 0)))

(define background-1 0)
(define background-2 1)
(define background-3 2)

(define foreground-1 5)
(define foreground-2 6)
(define foreground-3 7)

(define (build-game-object sprite)
  "build a game object from a sprite"
  (make <game-object>
    'sprite sprite
    'ul-row 0
    'ul-col 0
    'z foreground-1))

(define (increment-frame gobj)
  (let* ((sprite (slot-ref gobj 'sprite))
	 (frame-count (slot-ref sprite 'frame-count))
	 (current-frame (slot-ref sprite 'current-frame))
	 (next-frame (+ current-frame 1)))

    (if (= next-frame frame-count)
	(set! next-frame 0))

    (slot-set! sprite 'current-frame next-frame)))


(define (dump-game-object obj)
  "display the current frame of a game object"
  (dovector (row (current-frame obj))
    (display row)
    (newline)))


(define (draw-frame frame ul-row ul-col)
  "draw frame starting with its upper left corner at the coordinates
specified"
  (let ((row-number 0)
	(width (width frame))
	(str (make-string 2)))
    (dovector (row frame)
      (let ((current-row (+ ul-row row-number)))
	(let char-loop ((idx 0))
	  (when (< idx width)
  	    ;; spaces are transparent so don't draw them
	    (when (not (eq? (string-ref row idx) #\space))
	      ;; build a null terminated string from the char
	      ;; for mvprintw
	      (string-set! str 0 (string-ref row idx))
	      ;(display (list current-row
		;	     (+ ul-col idx)
		;	     str)) (newline))
	      (nc:mvprintw current-row
			   (+ ul-col idx)
			   str))
	    (char-loop (+ idx 1)))))
      (inc! row-number))))

(define (clear-rect ul-row ul-col rows cols)
  (dotimes (rr rows)
    (dotimes (cc cols)
      (nc:mvprintw (+ ul-row rr)
		   (+ ul-col cc)
		   " "))))

(define-class <view> ()
  "contains the camera information")

(define-class <curses-view> (<view>)
  "an advanced view that supports cursor motion"
  ('screen-ul-row
   'screen-ul-col))

(define-class <boring-view> (<view>)
  "a view that just dumps graphics straight out. good for debugging"
  ())

(set! bview (make <boring-view>))
(set! cview (make <curses-view>
	      'screen-ul-row 0
	      'screen-ul-col 0))

(define-generic upper-left-row
  "computes the row of the top left corner")
(define-generic upper-left-col
  "computes the column of the top left corner")

(define-method (upper-left-row (gobj <game-object>)
			       (view <curses-view>))
  (- (slot-ref gobj 'ul-row)
     (slot-ref view 'screen-ul-row)))

(define-method (upper-left-col (gobj <game-object>)
			       (view <curses-view>))
  (- (slot-ref gobj 'ul-col)
     (slot-ref view 'screen-ul-col)))

(define-generic draw
  "draw a thing")

(define-method (draw (gobj <game-object>) (view <curses-view>))
  (let ((ul-row (upper-left-row gobj view))
	(ul-col (upper-left-col gobj view))
	(frame (current-frame gobj)))

    (draw-frame frame ul-row ul-col)))

(define-method (draw (gobj <game-object>) (view <boring-view>))
  (dump-game-object gobj))

(define (z-order obj1 obj2)
  "defines an ordering among objects for drawing back to front"
  (<= (slot-ref obj1 'z)
      (slot-ref obj2 'z)))

(define (bottom-bound objs view)
  (reduce max (map (lambda (obj)
		     (+ (upper-left-row obj view)
			(height obj))) objs)))

(define (right-bound objs view)
  (reduce max (map (lambda (obj)
		     (+ (upper-left-col obj view)
			(width obj))) objs)))

(require "shiney/game-objects.sch")

(define (test)
  (with-curses win
    (nc:noecho)
    (nc:cbreak)
    (nc:keypad win #t)
    (nc:halfdelay 1)

    ;; clear the screen initially
    (nc:clear)

    (let* ((done #f)
	   (view (make <curses-view>
		   'screen-ul-row 0
		   'screen-ul-col 0))
	   (player (build-game-object waiter-sprite))
	   (bottom (bottom-bound scene view)))

      (push! player scene)
      (set! scene (sort-list z-order scene))
      (slot-set! player 'ul-row
		 (- bottom
		    (height player)))

      (dowhile (not done)
	;; get the input
	(let ((chr (nc:getch))
	      (rr (slot-ref player 'ul-row))
	      (cc (slot-ref player 'ul-col)))

	  (cond
	   ((= nc:key-left chr)
	    (slot-set! player 'ul-col (- cc 1))
	    (increment-frame player))
	   ((= nc:key-right chr)
	    (slot-set! player 'ul-col (+ cc 1))
	    (increment-frame player))
	   ((= nc:key-up chr)
	    (slot-set! player 'ul-row (- rr 1))
	    (increment-frame player))
	   ((= nc:key-down chr)
	    (slot-set! player 'ul-row (+ rr 1))
	    (increment-frame player))
	   ((= nc:key-enter chr)
	    (set! done #t))))

	(nc:clear)
	(dolist (obj scene)
          (draw obj view))
	(nc:mvprintw (+ bottom 1)
		     15
		     "Arrow keys move. Enter exits.")
	(nc:refresh)))))



