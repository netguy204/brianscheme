; Copyright 2010 Brian Taylor
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;

(require 'ffi)

(with-library (nc "libncurses")
  (let ((initscr (ffi:dlsym nc "initscr"))
	(noecho (ffi:dlsym nc "noecho"))
	(cbreak (ffi:dlsym nc "cbreak"))
	(keypad (ffi:dlsym nc "keypad"))
	(getch (ffi:dlsym nc "getch"))
	(clear (ffi:dlsym nc "clear"))
	(mvprintw (ffi:dlsym nc "mvprintw"))
	(addch (ffi:dlsym nc "addch"))
	(mvaddch (ffi:dlsym nc "mvaddch"))
	(move (ffi:dlsym nc "move"))
	(refresh (ffi:dlsym nc "refresh"))
	(nodelay (ffi:dlsym nc "nodelay"))
	(halfdelay (ffi:dlsym nc "halfdelay"))
	(curs-set (ffi:dlsym nc "curs_set"))
	(getmaxx (ffi:dlsym nc "getmaxx"))
	(getmaxy (ffi:dlsym nc "getmaxy"))
	(endwin (ffi:dlsym nc "endwin")))

    (define nc:stdwin
      (ffi:deref (ffi:dlsym-var nc "stdscr")))

    (define nc:err -1)

    (define nc:key-down 258)
    (define nc:key-up 259)
    (define nc:key-left 260)
    (define nc:key-right 261)
    (define nc:key-enter 10)

    (define (nc:initscr)
      "initialize the ncurses library"
      (ffi:funcall initscr 'ffi-pointer))

    (define (nc:noecho)
      "disable echoing of getch'd characters as they're typed"
      (ffi:funcall noecho 'ffi-uint))

    (define (nc:cbreak)
      "disable line buffering"
      (ffi:funcall cbreak 'ffi-uint))

    (define (nc:keypad screen boolean)
      "treat function keys (like arrows) specially"
      (ffi:funcall keypad 'ffi-uint
		   screen (if boolean
			      (ffi:alien-uchar 1)
			      (ffi:alien-uchar 0))))

    (define (nc:getch)
      "read a character from the window"
      (ffi:funcall getch 'ffi-uint))

    (define (nc:clear)
      "clear the screen"
      (ffi:funcall clear 'ffi-uint))

    (define (nc:mvprintw x y msg)
      "show msg at position x y"
      (ffi:funcall mvprintw 'ffi-uint
		   (ffi:alien-uint x)
		   (ffi:alien-uint y)
		   (ffi:alien-string msg)))

    (define (nc:addch ch)
      "show char at current position"
      (ffi:funcall addch 'ffi-uint
		   (ffi:alien-uchar (char->integer ch))))

    (define (nc:mvaddch x y ch)
      "show char at position x y"
      (ffi:funcall mvaddch 'ffi-uint
		   (ffi:alien-uint x)
		   (ffi:alien-uint y)
		   (ffi:alien-uchar (char->integer ch))))

    (define (nc:move x y)
      "set current position to x y"
      (ffi:funcall move 'ffi-uint
		   (ffi:alien-uint x)
		   (ffi:alien-uint y)))

    (define (nc:refresh)
      "flush the ncurses state to the terminal"
      (ffi:funcall refresh 'ffi-uint))

    (define (nc:nodelay screen boolean)
      "cause getch not to block if no data is available"
      (ffi:funcall nodelay 'ffi-uint
		   screen (if boolean
			      (ffi:alien-uchar 1)
			      (ffi:alien-uchar 0))))

    (define (nc:halfdelay wait)
      "allow getch to block for data up to time milliseconds"
      (ffi:funcall halfdelay 'ffi-uint wait))

    (define (nc:curs-set visibility)
      "0 - invisible, 1 - normal, 2 - very visible"
      (ffi:funcall curs-set 'ffi-uint visibility))

    (define (nc:getmaxx win)
      "Get screen width."
      (ffi:funcall getmaxx 'ffi-uint win))

    (define (nc:getmaxy win)
      "Get screen height."
      (ffi:funcall getmaxy 'ffi-uint win))

    (define (nc:endwin)
      "return the terminal to normal mode"
      (ffi:funcall endwin 'ffi-uint))))

(define-syntax (with-curses win . body)
  "wrap body in initscr and endwin"
  `(begin
     (let* ((,win (nc:initscr))
	    (result (begin . ,body)))
       (nc:endwin)
       result)))

(define (nc:debug-write form)
  "since curses trashes stdout, this is a handy way to find out what's
happening"
  (let ((out (open-output-port "debug")))
    (write-port form out)
    (close-output-port out)))

(define (nc:demo)
  "simple demo of curses. doesn't seem to return with the repl fully
intact though."
  (with-curses win
    (nc:noecho)
    (nc:cbreak)
    (nc:keypad win #t)
    (nc:halfdelay 1)

    (let ((boundx 80)
	  (boundy 40)
	  (mx 10)
	  (my 10)
	  (bx 0)
	  (by 0)
	  (bxv 1)
	  (byv 1)
	  (end #f))

      (dowhile (not end)
	;; get the input
	(let ((chr (nc:getch)))
	  (nc:debug-write chr)
	  (cond
	   ((= nc:key-left chr)
	    (set! mx (max 0 (- mx 1))))
	   ((= nc:key-right chr)
	    (set! mx (min 40 (+ mx 1))))
	   ((= nc:key-up chr)
	    (set! my (max 0 (- my 1))))
	   ((= nc:key-down chr)
	    (set! my (min 40 (+ my 1))))
	   ((= nc:key-enter chr)
	    (set! end #t))))

	;; update the ball
	(when (or (> bx boundx)
		  (< bx 0))
	  (set! bxv (- bxv)))

	(when (or (> by boundy)
		  (< by 0))
	  (set! byv (- byv)))

	(set! bx (+ bx bxv))
	(set! by (+ by byv))

	;; clear and draw the screen
        (nc:clear)
	(nc:mvprintw
	 my mx
	 (string-append "hello world "
		 (number->string mx)
		 " "
		 (number->string my)))
	(nc:mvprintw by bx "#")
	(nc:refresh)))


    (sleep 1)))


