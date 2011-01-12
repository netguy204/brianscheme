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

(define waiter-sprite
  (build-sprite waiter-graphic-1
		waiter-graphic-2
		waiter-graphic-3))

(define table-graphic
  #(" ............ "
    "/------------\\"
    "||          ||"
    "||          ||"))

(define table-sprite
  (build-sprite table-graphic))

(define window-graphic
  #("/-----\\"
    "|     |"
    "\\=====/"))

(define window-sprite
  (build-sprite window-graphic))

(define door-graphic
  #("       "
    "       "
    "+-----+"
    "|     |"
    "|     |"
    "|     |"
    "|     |"
    "======="))

(define door-sprite
  (build-sprite door-graphic))

(define letter-codes
  (list (list #\w window-sprite background-1)
	(list #\t table-sprite background-3)
	(list #\d door-sprite background-1)))

(define scene-col-step 10)
(define scene-row-step 5)

(define coded-scene
  #(" wd  w "
    "t  t  t"))

(define (scene-to-objects scene)
  (let ((result nil)
	(row-num 0)
	(width (string-length (vector-ref scene 0))))
    (dovector (row scene)
      (let char-loop ((col-num 0))
	(unless (= col-num width)
	  (let ((code (string-ref row col-num)))
	    (if (not (eq? code
			  #\space))
		(let ((gobj (build-game-object
			     (second (assoc code letter-codes)))))
		  (slot-set! gobj 'ul-row
			     (* row-num scene-row-step))
		  (slot-set! gobj 'ul-col
			     (* col-num scene-col-step))
		  (slot-set! gobj 'z
			     (third (assoc code letter-codes)))
		  (push! gobj result))))
	  (char-loop (+ col-num 1))))
      (inc! row-num))
    result))


(define scene (scene-to-objects coded-scene))

