; Side-scrolling space shoot-'em-up using ncurses, CLOS, and Pth

(require 'clos)
(require 'pth)
(require 'ncurses)

(define random-seed 168230232)

(define (random n)
  "Generate a 16-bit random number using the middle-square method."
  (set! random-seed (logand 65535 (ash (* random-seed random-seed) -7)))
  (if n
      (mod random-seed n)
      random-seed))

(define objects '())
(define game-speed 100) ; ms
(define initial-ships 10)
(define shot-speed 4)
(define width 80)
(define height 24)
(define map-factor 4)
(define map-width (* width map-factor))
(define map-height (* height map-factor))
(define done #f)

(define-class <ship> ()
  "A spaceship."
  ('x 'y 'hp 'symbol 'speed))

(define-class <shot> ()
  "A shot from a spaceship."
  ('x 'y 'speed))

(define-method (initialize (ship <ship>))
  (slot-set! ship 'x map-width)
  (slot-set! ship 'y (random map-height))
  (slot-set! ship 'hp 1)
  (slot-set! ship 'speed -1)
  (slot-set! ship 'symbol "<"))

(define-generic draw
  "Draw an object to the screen.")

(define-method (draw (ship <ship>))
  (nc:mvprintw (/ (slot-ref ship 'y) map-factor)
	       (/ (slot-ref ship 'x) map-factor)
	       (slot-ref ship 'symbol)))

(define-method (draw (shot <shot>))
  (nc:mvprintw (/ (slot-ref shot 'y) map-factor)
	       (/ (slot-ref shot 'x) map-factor)
	       "-"))

(define-generic erase
  "Erase an object from the screen.")

(define-method (erase (ship <ship>))
  (nc:mvprintw (/ (slot-ref ship 'y) map-factor)
	       (/ (slot-ref ship 'x) map-factor)
	       " "))

(define-method (erase (shot <shot>))
  (nc:mvprintw (/ (slot-ref shot 'y) map-factor)
	       (/ (slot-ref shot 'x) map-factor)
	       " "))

(define-generic shoot
  "Shoot a new shot.")

(define-method (shoot (ship <ship>))
  (push! (make <shot>
	   'x (slot-ref ship 'x)
	   'y (slot-ref ship 'y)
	   'speed (* shot-speed (slot-ref ship 'speed))) objects))

(define-generic step
  "Advance object in simulation.")

(define-method (step (ship <ship>))
  (slot-set! ship 'x (+ (slot-ref ship 'x) (slot-ref ship 'speed)))
  (if (= 0 (random 50)) (shoot ship))
  (if (or (> (slot-ref ship 'x) map-width)
	  (< (slot-ref ship 'x) 0))
      (remove ship)))

(define-method (step (shot <shot>))
  (slot-set! shot 'x (+ (slot-ref shot 'x) (slot-ref shot 'speed)))
  (if (or (> (slot-ref shot 'x) map-width)
	  (< (slot-ref shot 'x) 0))
      (remove shot)))

(define (spawn-ship)
  "Add a new enemy ship into the game."
  (push! (make <ship>) objects))

(define (remove object)
  "Remove object from the game."
  (set! objects (delq object objects)))

(define player (make <ship>))

(define (init-game)
  "Reinitialize game structures."
  (set! done #f)
  (set! objects '())
  (dotimes (i initial-ships)
    (spawn-ship))
  (slot-set! player 'speed 1)
  (slot-set! player 'x 0)
  (slot-set! player 'y (/ map-height 2))
  (slot-set! player 'hp 10)
  (slot-set! player 'symbol ">"))

(define (simulate-loop)
  (if (= 1 (random 10))
      (spawn-ship))
  (erase-map)
  (sim-step)
  (draw-map)
  (pth:usleep (* game-speed 1000))
  (if (not done)
      (simulate-loop)))

(define (control-player)
  (let ((chr (pth:getch)))
    (erase player)
    (cond
     ((= nc:key-left chr)
      (move 'x -1))
     ((= nc:key-right chr)
      (move 'x 1))
     ((= nc:key-up chr)
      (move 'y -1))
     ((= nc:key-down chr)
      (move 'y 1))
     ((= (char->integer #\ ) chr)
      (shoot player))
     ((= (char->integer #\q) chr)
      (set! done #t))))
  (draw-map)
  (if (not done)
      (control-player)))

(define (move slot n)
  "Move the player in the given direction."
  (let* ((p (slot-ref player slot))
	 (new-p (+ p (* n map-factor)))
	 (max (if (eq? slot 'x) map-width map-height)))
    (if (< new-p 0)
	(slot-set! player slot 0)
	(if (> new-p max)
	    (slot-set! player slot max)
	    (slot-set! player slot new-p)))))

(define (erase-map)
  (for-each erase objects)
  (erase player))

(define (draw-map)
  (nc:mvprintw 0 0 (number->string (length objects)))
  (for-each draw objects)
  (draw player)
  (nc:refresh))

(define (sim-step)
  (dolist (object objects)
    (step object)))

(define (play-game)
  (init-game)
  (with-curses win
    (nc:noecho)
    (nc:cbreak)
    (nc:keypad win #t)
    (nc:curs-set 0)
    (pth:spawn simulate-loop)
    (control-player)))
