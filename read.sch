;;; read.sch --- a flexible BS reader

;; This reader takes over for the C reader, being much more flexible.

;; General functions

(define *whitespace-characters* (string->list " \n	"))

(define (whitespace? ch)
  "Return #t if character is whitespace."
  (member? ch *whitespace-characters*))

(define (paren? ch)
  "Return #t if character is parenthesis."
  (or (eq? ch #\()
      (eq? ch #\))))

(define (digit? ch)
  "Return #t if character is a digit."
  (member? ch (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(define (read-char-safe port)
  "Throw an error if read returns eof-object."
  (let ((ch (read-char port)))
    (if (eof-object? ch)
	(throw-error "unexpected eof" "eof")
	ch)))

(define (count-member el lst)
  "Count the number of times an element appears in a list."
  (let ((mem (member el lst)))
    (if mem
	(+ 1 (count-member el (cdr mem)))
	0)))

;; Reader macros

(define *macro-characters* '()
  "Characters that dispatch reader macros when read at the beginning
of a token.")

(define (set-macro-character! ch fn)
  "Add reader macro for the given character."
  (set! *macro-characters* (assq-set! *macro-characters* ch fn)))

(define-syntax (define-macro-character char-and-port . body)
  "Define-style syntax for creating reader macros."
  (let ((char (first char-and-port))
	(port (second char-and-port)))
    `(set-macro-character! ,char
			   (lambda (,port)
			     ,@body))))

(define (macro-character? ch)
  "Return #t if character is a macro character."
  (assq ch *macro-characters*))

(define (get-macro-character ch)
  "Return the macro character function for the character."
  (cdr (assq ch *macro-characters*)))

(define *dispatch-macro-characters* '()
  "Character macros that dispatch on #.")

(define (set-dispatch-macro-character! ch fn)
  "Add # reader macro for the given character."
  (set! *dispatch-macro-characters*
	(assq-set! *dispatch-macro-characters* ch fn)))

(define (get-dispatch-macro-character ch)
  "Return the macro character function for the character."
  (cdr (assq ch *dispatch-macro-characters*)))

(define-macro-character (#\# port)
  (let* ((ch (read-char-safe port))
	 (fn (get-dispatch-macro-character ch)))
    (if (not fn)
	(throw-error "unknown dispatch # macro" ch)
	(fn port))))

(define-syntax (define-dispatch-macro-character char-and-port . body)
  "Define-style syntax for creating dispatch reader macros on #."
  (let ((char (first char-and-port))
	(port (second char-and-port)))
    `(set-dispatch-macro-character! ,char
				    (lambda (,port)
				      ,@body))))
;; Define some reader macros

(define-macro-character (#\' port)
  "Quote reader macro."
  (list 'quote (read:read port)))

(define-macro-character (#\` port)
  "Quasiquote reader macro."
  (list 'quasiquote (read:read port)))

(define-macro-character (#\, port)
  "Unquote reader macro."
  (let ((ch (read-char-safe port)))
    (cond
     ((eq? #\@ ch) (list 'unquotesplicing (read:read port)))
     (#t (begin (unread-char ch port)
		(list 'unquote (read:read port)))))))

(define-macro-character (#\" port)
  "String reader macro."
  (read:slurp-atom port 'stop? (lambda (ch) (eq? #\" ch))
		   'allow-eof #f))

(set-dispatch-macro-character! #\t (always #t))
(set-dispatch-macro-character! #\f (always #f))
(set-dispatch-macro-character! #\! read-line)

(define-dispatch-macro-character (#\( port)
  "Read in a vector."
  (apply vector (read:list port)))

(define-dispatch-macro-character (#\\ port)
  "Read a character."
  (let ((ch (read-char-safe port))
	(peek (peek-char port)))
    (cond
     ((and (eq? ch #\n) (eq? peek #\e))
      (begin (read:slurp-atom port) #\newline))
     ((and (eq? ch #\s) (eq? peek #\p))
      (begin (read:slurp-atom port) #\space))
     ((and (eq? ch #\t) (eq? peek #\a))
      (begin (read:slurp-atom port) #\tab))
     (#t ch))))

;; Token predicates

(define (read:lp? token)
  "Is token the left parenthesis?"
  (eq? (car token) 'lp))

(define (read:rp? token)
  "Is token the right parenthesis?"
  (eq? (car token) 'rp))

(define (read:dot? token)
  "Is this the dot operator?"
  (eq? (car token) 'dot))

(define (read:obj? token)
  "Is token a Lisp object?"
  (eq? (car token) 'obj))

(define (read:eof? token)
  (eq? (car token) 'eof))

;; Read functions

(define (read:read port)
  "Read an s-expression or object from the port."
  (let ((token (read:token port)))
    (cond
     ((read:lp? token) (read:list port))
     ((read:rp? token) (throw-error "read unexpected ')'" token))
     ((read:obj? token) (cdr token))
     ((read:eof? token) (cdr token)))))

(define (read:list port)
  "Read a list from the given port, assuming opening paren is gone."
  (let ((token (read:token port)))
    (cond
     ((read:lp? token) (cons (read:list port) (read:list port)))
     ((read:rp? token) '())
     ((read:dot? token) (let ((end (read:read port)))
			  (unless (read:rp? (read:token port))
				  (throw-error "missing expected ')'" "."))
			  end))
     ((read:obj? token) (cons (cdr token) (read:list port)))
     ((read:eof? token) (throw-error "unexpected eof" token)))))

(define (read:token port)
  "Read the next token from the port."
  (let ((ch (read-char port)))
    (cond
     ((eof-object? ch) (cons 'eof ch))
     ((macro-character? ch) (cons 'obj ((get-macro-character ch) port)))
     ((whitespace? ch) (read:token port))
     ((eq? ch #\;) (begin (read:eat-comment port) (read:token port)))
     ((eq? #\( ch) (cons 'lp ch))
     ((eq? #\) ch) (cons 'rp ch))
     ((and (eq? #\. ch) (whitespace? (peek-char port))) (cons 'dot ch))
     (#t (cons 'obj (begin (unread-char ch port)
			   (read:from-token (read:slurp-atom port))))))))

(define (read:slurp-atom port (stop? whitespace?) (allow-eof #t))
  "Read until the next whitespace."
  (let ((ch (read-char port)))
    (cond
     ((eof-object? ch) (if allow-eof "" (throw-error "unexpected eof" "")))
     ((stop? ch) "")
     ((and allow-eof (eq? ch #\;)) (begin (read:eat-comment port) ""))
     ((and allow-eof (paren? ch)) (begin (unread-char ch port) ""))
     ((eq? ch #\\) (string-append (char->string (read:escaped port))
				  (read:slurp-atom port 'stop? stop?
						   'allow-eof allow-eof)))
     (#t (string-append (char->string ch)
			(read:slurp-atom port 'stop? stop?
					 'allow-eof allow-eof))))))

(define read:eat-comment read-line
  "Consume stream until the end of the line.")

(define (read:escaped port)
  "Read an escaped character, and throw an error on EOF."
  (let ((ch (read-char-safe port)))
    (cond
     ((eq? ch #\n) #\newline)
     ((eq? ch #\t) #\tab)
     (#t ch))))

(define (read:from-token str)
  "Turn the token in the string into either an integer, real, or symbol."
  (let* ((lst (string->list str))
	 (signed (or (eq? (car lst) #\-) (eq? (car lst) #\+))))
    (cond
     ((and (or (and signed
		    (every? digit? (cdr lst))
		    (not (null? (cdr lst))))
	       (and (not signed)
		    (every? digit? lst)))
	   (string->number str)))	; integer
     ((and (or (and signed (every? digit? (delq #\. (cdr lst)))
		    (not (null? (cdr lst))))
	       (and (not signed) (every? digit? (delq #\. lst))
		    (not (null? (delq #\. lst)))))
	   (= 1 (count-member #\. lst))
	   (every? digit? (delq #\. (cdr lst))))
      (string->number str))		; real
     (#t (string->symbol str)))))	; everything else is a symbol
