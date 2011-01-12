; DESCRIPTION: Displays a counter, and an extra message when any key
; is pressed.

; NOTE: This test requires ncurses because I want to use
; getch(). Without threads the only way to implement this would be
; with halfdelay(), and that still wouldn't behave quite right due to
; the minimal delay.

(require 'pth)
(require 'ncurses)

(define counter 0)
(define getch-event (pth:event 4098 0))

(define (hello)
  (inc! counter)
  (nc:mvprintw 1 0 "       ")
  (nc:mvprintw 0 0 (concat "hello " (number->string counter)))
  (nc:refresh)
  (pth:usleep (* 1400 1000))
  (hello))

(define (goodbye)
  (pth:wait getch-event)
  (nc:getch)
  (nc:mvprintw 1 0 "goodbye")
  (nc:refresh)
  (goodbye))

(with-curses win
 (nc:noecho)
 (nc:cbreak)
 (pth:spawn hello)
 (pth:join (pth:spawn goodbye)))
