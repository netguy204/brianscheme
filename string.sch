(require 'math)

(define (string-append . args)
  "join a series of strings"
  (if (null? args)
      ""
      (reduce prim-concat args)))

(define (string-length str)
  "find the length of a string, not including null"
  (do ((len 0 (+ len 1)))
      ((= (char->integer (string-ref str len)) 0) len)))

(define (substring str start . end)
  "Return given substring from start (inclusive) to end (exclusive)."
  (let* ((strlen (string-length str))
	 (end (car-else end strlen))
	 (len (- end start)))
    (when (or (> len strlen) (> end strlen) (> start strlen))
	  (throw-error "out of string bounds" str start end))
    (when (> start end)
	  (throw-error "invalid substring" start end))
    (let ((substr (make-string len #\.)))
      (dotimes (i len)
        (string-set! substr i (string-ref str (+ i start))))
      substr)))

(define (string-map fn str)
  "map a function over all characters in a string to produce a new
string"
  (let* ((len (string-length str))
	 (new (make-string len #\space)))
    (dotimes (idx len)
      (string-set! new idx
		   (fn (string-ref str idx))))
    new))

(define (char-case-switch char uppercase?)
  "change the case of char if alpha."
  (let* ((ord (char->integer char))
	 (a (if uppercase?
		(char->integer #\a)
		(char->integer #\A)))
	 (z (if uppercase?
		(char->integer #\z)
		(char->integer #\Z)))
	 (step (- (char->integer #\A)
		  (char->integer #\a)))
	 (step (if uppercase?
		   step
		   (- step))))
    (if (and (>= ord a)
	     (<= ord z))
	(integer->char (+ ord step))
	char)))

(define (char-uppercase char)
  "convert an alpha character to uppercase if it isn't
already. ignores non-alpha"
  (char-case-switch char #t))

(define (char-lowercase char)
  "convert an alpha character to lowercase if it isn't
already. ignores non-alpha"
  (char-case-switch char #f))

(define (alpha? char)
  "true if char is a letter"
  (let ((ord (char->integer (char-lowercase char)))
	(a (char->integer #\a))
	(z (char->integer #\z)))
    (and (>= ord a)
	 (<= ord z))))

(define (uppercase str)
  "convert a string to uppercase"
  (string-map char-uppercase str))

(define (lowercase str)
  "convert a string to lowercase"
  (string-map char-lowercase str))

(define (string->list str)
  "Turn a string into a character list."
  (let ((len (string-length str)))
    (if (= 0 len)
	nil
	(do ((idx 0 (+ idx 1))
	     (result nil (cons (string-ref str idx)
			       result)))
	    ((= idx len) (reverse result))))))

(define (list->string lst)
  "Convert a list-string into a string."
  (let* ((len (length lst))
         (str (make-string len ".")))
    (letrec ((iter (lambda (lst str n)
                     (if (null? lst)
                         str
                         (begin
                           (string-set! str n (car lst))
                           (iter (cdr lst) str (+ n 1)))))))
      (iter lst str 0))))

(define (string=? . args)
  "Return #t if all strings arguments are equal?."
  (every-pair? equal? args))

(define (char->string char)
  "Return a string containing only char."
  (make-string 1 char))

;; String number processing

(define *digits* (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define *digits-16* (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                          #\a #\b #\c #\d #\e #\f))

(define (digit? ch)
  "Return #t if character is a digit."
  (member? ch *digits*))

(define (integer-string-list? lst digits)
  "Return #t if string-list contains an integer."
  (and (every? (rcurry member? digits) (cdr lst))
       (or (and (or (eq? (car lst) #\-)
                    (eq? (car lst) #\+))
                (not (null? (cdr lst))))
           (member? (car lst) digits))))

(define (integer-string? string)
  "Return #t if string contains an integer."
  (integer-string-list? (string->list string) *digits*))

(define (string-list->integer lst base)
  "Convert the integer in the string-list to an integer."
  (letrec ((iter (lambda (number lst)
                   (if (null? lst)
                       number
                       (iter (+ (* base number)
                                (index-of (curry eq? (car lst)) *digits-16*))
                             (cdr lst))))))
    (cond
     ((eq? (car lst) #\-) (- (iter 0 (cdr lst))))
     ((eq? (car lst) #\+) (iter 0 (cdr lst)))
     (#t (iter 0 lst)))))

(define (string->integer string (base 10))
  "Convert the integer in the string to an integer."
  (let ((lst (string->list string)))
    (if (not (integer-string-list?
              lst (reverse (nthcdr (- 16 base) (reverse *digits-16*)))))
	(error "invalid integer in string" string)
        (string-list->integer lst base))))

(define (real-string-list? lst)
  "Return #t if string-list contains a real."
  (letrec ((iter (lambda (lst saw-dot saw-digit)
                   (cond
                    ((null? lst) (and saw-dot saw-digit))
                    ((eq? (car lst) #\e) (if saw-digit
					     (integer-string-list?
					      (cdr lst) *digits*)
					     #f))
                    ((eq? (car lst) #\.)
                     (if saw-dot
                         #f
                         (iter (cdr lst) #t saw-digit)))
                    ((digit? (car lst)) (iter (cdr lst) saw-dot #t))
                    (#t #f)))))
    (if (or (eq? (car lst) #\-)
            (eq? (car lst) #\+))
        (iter (cdr lst) #f #f)
        (iter lst #f #f))))

(define (real-string? string)
  "Return #t if string contains a real."
  (real-string-list? (string->list string)))

(define (string-list->real lst)
  "Convert real in string-list to real."
  (letrec ((iter (lambda (number lst m1 m2 saw-dot)
                   (cond
                    ((null? lst) number)
                    ((digit? (car lst))
                     (iter (+ (* m1 number)
                              (* m2 (index-of (curry eq? (car lst))
                                              *digits*)))
                           (cdr lst) m1 (if saw-dot (/ m2 10) m2) saw-dot))
                    ((eq? (car lst) #\e)
                     (* number (expt 10.0 (string-list->integer
                                           (cdr lst) 10))))
                    ((eq? (car lst) #\.)
                     (iter number (cdr lst) 1.0 0.1 #t))))))
    (cond
     ((eq? (car lst) #\-) (- (iter 0 (cdr lst) 10.0 1.0 #f)))
     ((eq? (car lst) #\+) (iter 0 (cdr lst) 10.0 1.0 #f))
     (#t(iter 0 lst 10.0 1.0 #f)))))

(define (string->real string)
  "Convert real in string to real."
  (let ((lst (string->list string)))
    (if (not (real-string-list? lst))
        (error "invalid real in string" string)
        (string-list->real lst))))


(define (string-left-pad val char tgt-len)
  "pad the left side of string VAL with CHAR until it's TGT-LEN"
  (let ((len (string-length val)))
    (if (< len tgt-len)
	(string-append (make-string (- tgt-len len) char)
		       val)
	val)))

(define (integer->string int (base 10) (pad 0))
  "Convert integer into a string, with optional base."
  (assert-types (int integer?))
  (letrec ((iter (lambda (n lst)
                   (if (zero? n)
                       lst
                       (iter (/ n base)
                             (cons (list-ref *digits-16* (mod n base)) lst))))))
    (if (zero? int)
        (string-left-pad "0" #\0 pad)
        (string-left-pad (list->string (iter int '())) #\0 pad))))

(define (chomp line)
  (let ((len (string-length line)))
    (if (eq? (string-ref line (- len 1))
	     #\newline)
	(if (and (> len 1)
		 (= (char->integer (string-ref line (- len 2)))
		    13))
	    (substring line 0 (- len 2))
	    (substring line 0 (- len 1)))
	line)))

(define (trim line)
  (let loop ((idx 0))
    (let ((char (string-ref line idx)))
      (cond
       ((= (char->integer char) 0)
	(list line nil))
       ((eq? char #\space)
	(loop (+ idx 1)))
       (else
	(substring line idx))))))

(define (string-split line split-char)
  (let loop ((idx 0))
    (let ((char (string-ref line idx)))
      (cond
       ((= (char->integer char) 0)
	(list line nil))
       ((eq? char split-char)
	(list (substring line 0 idx)
	      (substring line (+ idx 1))))
       (else (loop (+ idx 1)))))))
