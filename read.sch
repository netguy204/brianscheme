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

;; Define some reader macros

(define-macro-character (#\' port)
  "Quote reader macro."
  (list 'quote (read:read port)))

;; Token predicates

(define (read:lp? token)
  "Is token the left parenthesis?"
  (eq? (car token) 'lp))

(define (read:rp? token)
  "Is token the right parenthesis?"
  (eq? (car token) 'rp))

(define (read:obj? token)
  "Is token a Lisp object?"
  (eq? (car token) 'obj))

(define (read:eof? token)
  (eq? (car token) 'eof))

;; Read functions

(define read:from-token identity) ; TODO

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
     ((read:obj? token) (cons (cdr token) (read:list port)))
     ((read:eof? token) (throw-error "unexpected eof" token)))))

(define (read:token port)
  "Read the next token from the port."
  (let ((ch (read-char port)))
    (cond
     ((eq? #\( ch) (cons 'lp ch))
     ((eq? #\) ch) (cons 'rp ch))
     ((whitespace? ch) (read:token port))
     ((eof-object? ch) (cons 'eof ch))
     (#t (cons 'obj (read:slurp-atom port ch))))))

(define (read:slurp-atom port . init)
  "Read until the next whitespace."
  (let ((ch (car-else init (read-char port))))
    (cond
     ((eof-object? ch) "")
     ((whitespace? ch) "")
     ((paren? ch) (begin (unread-char ch port) ""))
     ((eq? ch #\\) (string-append (char->string (read:escaped port))
				  (read:slurp-atom port)))
     ((macro-character? ch) ((get-macro-character ch) port))
     (#t (string-append (char->string ch) (read:slurp-atom port))))))

(define (read:escaped port)
  "Read an escaped character, and throw an error on EOF."
  (let ((ch (read-char port)))
    (if (eof-object? ch)
	(throw-error "unexpected eof during escape" "\\")
	ch)))
