;; "Something Shiney"
;;
;; An entry into the January 2011 lisp game programming expo
;;
;; Brian Taylor
;;


(require 'clos)
(require 'ncurses)

(define table-graphic
  #(" ............ "
    "/------------\\"
    "||          ||"
    "||          ||"))

(define waiter-graphic-1
  #(" O "
    "/| "
    " |\\"))

(define waiter-graphic-2
  #(" O "
    " |\\"
    " | "))

(define waiter-graphic-3
  #(" O "
    " | "
    "/| "))

(define-struct game-object
  "fundamental game object"
  ((image)
   (width)
   (height)))

(define (build-game-object image)
  (let ((exp-width (string-length (vector-ref image 0))))
    (dovector (row image)
	      (if (not (= (string-length row))
		       exp-width)
		  (throw-error "image had non-rectangular dimensions")))

    (make-game-object 'image image
		      'width exp-width
		      'height (vector-length image))))

(define (dump-game-object obj)
  (dovector (row (game-object-image obj))
	    (display row)
	    (newline)))

(define table (build-game-object table-graphic))
