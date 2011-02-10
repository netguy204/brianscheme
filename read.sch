;;; read.sch --- a flexible BS reader

;; DESCRIPTION: This new reader takes over for the C reader, being
;; much more flexible. It relies only the input stream providing
;; read-char and unread-char functionality.

;; General functions

(define (read:kill-line stream)
  "Read the next line from the input stream."
  (unless (eq? (read-stream-char stream) #\newline)
	  (read:kill-line stream)))

(define (peek-stream-char stream)
  "Look at the next character without removing it from the stream."
  (let ((ch (read-stream-char stream)))
    (unread-stream-char stream ch)
    ch))

(define *whitespace-characters* (string->list " \n\t"))

(define (whitespace? ch)
  "Return #t if character is whitespace."
  (member? ch *whitespace-characters*))

(define (read-stream-char-safe stream)
  "Throw an error if read returns eof-object."
  (let ((ch (read-stream-char stream)))
    (if (eof-object? ch)
	(throw-error "unexpected eof" "eof")
	ch)))

;; Reader macros

(define *readtable* '(() ())   ; functions first, sub-tables second
  "Characters that dispatch reader macros when read.")

(define (set-macro-character! ch fn)
  "Add reader macro for the given character."
  (set! *readtable* (list (assq-set! (first *readtable*) ch fn)
			  (second *readtable*))))

(define-syntax (define-macro-character char-and-stream . body)
  "Define-style syntax for creating reader macros."
  (let ((char (first char-and-stream))
	(stream (second char-and-stream)))
    `(set-macro-character! ,char
			   (lambda (,stream)
			     ,@body))))

(define (macro-character? ch)
  "Return #t if character is a macro character."
  (assq ch (first *readtable*)))

(define (get-macro-character ch)
  "Return the macro character function for the character."
  (cdr (assq ch (first *readtable*))))

(define (get-dispatch-macro-character ch1 ch2)
  "Return the macro character function for the character."
  (cdr (assq ch2 (cdr (assq ch1 (second *readtable*))))))

(define (make-dispatch-macro-character ch)
  (set! *readtable* (list (first *readtable*)
			  (assq-set! (second *readtable*) ch '())))
  (define-macro-character (ch stream)
    (let* ((ch2 (read-stream-char-safe stream))
	   (fn (get-dispatch-macro-character ch ch2)))
      (if fn
	  (fn stream)
	  (throw-error "unknown dispatch macro" ch2)))))

(define (set-dispatch-macro-character! ch1 ch2 fn)
  "Add dispatch reader macro for the given characters."
  (if (macro-character? ch1)
      (assq-set! (second *readtable*) ch1
		 (assq-set! (cdr (assq ch1 (second *readtable*))) ch2 fn))
      (throw-error "no dispatch macro" ch1)))

(define-syntax (define-dispatch-macro-character chars-and-stream . body)
  "Define-style syntax for creating dispatch reader macros on #."
  (let ((ch1 (first chars-and-stream))
	(ch2 (second chars-and-stream))
	(stream (third chars-and-stream)))
    `(set-dispatch-macro-character! ,ch1 ,ch2
				    (lambda (,stream)
				      ,@body))))

;; Define some reader macros

(make-dispatch-macro-character #\#)

(define-macro-character (#\' stream)
  "Quote reader macro."
  (list 'quote (read:read stream 'eof-error #t)))

(define-macro-character (#\` stream)
  "Quasiquote reader macro."
  (list 'quasiquote (read:read stream 'eof-error #t)))

(define-macro-character (#\, stream)
  "Unquote reader macro."
  (let ((ch (read-stream-char-safe stream)))
    (cond
     ((eq? #\@ ch) (list 'unquotesplicing (read:read stream 'eof-error #t)))
     (#t (begin (unread-stream-char stream ch)
		(list 'unquote (read:read stream 'eof-error #t)))))))

(define-macro-character (#\" stream)
  "String reader macro."
  (read:slurp-atom stream 'stop? (lambda (ch) (eq? #\" ch))
		   'allow-eof #f))

(define-macro-character (#\; stream)
  (read:kill-line stream))

(define-macro-character (#\( stream)
  (read:list stream #\)))

(define-macro-character (#\) stream)
  (throw-error "read unexpected ')'" #\)))

(define *dot* (string->symbol "."))

(set-dispatch-macro-character! #\# #\t (always #t))
(set-dispatch-macro-character! #\# #\f (always #f))
(set-dispatch-macro-character! #\# #\! read:kill-line)

(define-dispatch-macro-character (#\# #\( stream)
  "Read in a vector."
  (apply vector (read:list stream #\))))

(define-dispatch-macro-character (#\# #\\ stream)
  "Read a character."
  (let ((ch (read-stream-char-safe stream))
	(peek (peek-stream-char stream)))
    (cond
     ((and (eq? ch #\n) (eq? peek #\e))
      (begin (read:slurp-atom stream) #\newline))
     ((and (eq? ch #\s) (eq? peek #\p))
      (begin (read:slurp-atom stream) #\space))
     ((and (eq? ch #\t) (eq? peek #\a))
      (begin (read:slurp-atom stream) #\tab))
     (#t ch))))

(define-dispatch-macro-character (#\# #\B stream)
  (string->integer (read:slurp-atom stream) 'base 2))
(set-dispatch-macro-character! #\# #\b (get-dispatch-macro-character #\# #\B))

(define-dispatch-macro-character (#\# #\O stream)
  (string->integer (read:slurp-atom stream) 'base 8))
(set-dispatch-macro-character! #\# #\o (get-dispatch-macro-character #\# #\O))

(define-dispatch-macro-character (#\# #\X stream)
  (string->integer (read:slurp-atom stream) 'base 16))
(set-dispatch-macro-character! #\# #\x (get-dispatch-macro-character #\# #\X))

(define-dispatch-macro-character (#\# #\: stream)
  (string->uninterned-symbol (read:slurp-atom stream)))

(define-dispatch-macro-character (#\# #\. stream)
  (eval (read stream 'eof-error #t)))

(define-dispatch-macro-character (#\# #\< stream)
  "Produce an error."
  (throw-error "unreadable object" "#<...>"))

;; Read functions

(define (read:read stream (eof-error #f))
  "Read an s-expression or object from the stream."
  (let ((flush (read:flush-whitespace stream)))
    (if (eof-object? flush)
	flush
	(let ((ch (read-stream-char stream)))
	  (cond
	   ((eof-object? ch) (if eof-error
				 (throw-error "unexpected eof" ch)
				 ch))
	   ((macro-character? ch) ((get-macro-character ch) stream))
	   (#t (begin
		 (unread-stream-char stream ch)
		 (read:from-token (read:slurp-atom stream)))))))))

(define (read:flush-whitespace stream)
  "Eat all the whitespace, including comments, coming up in the stream."
  (let ((ch (read-stream-char stream)))
    (if (eq? ch #\;)
        (begin
          (read:kill-line stream)
          (read:flush-whitespace stream))
        (if (whitespace? ch)
            (read:flush-whitespace stream)
            (if (eof-object? ch)
		ch
		(unread-stream-char stream ch))))))

(define (read:list stream char)
  "Read a list from the given stream, assuming opening char is gone."
  (if (eof-object? (read:flush-whitespace stream))
      (throw-error "unexpected eof" "eof"))
  (let ((ch (read-stream-char-safe stream)))
    (cond
     ((eq? ch char) '())
     (#t (begin
           (unread-stream-char stream ch)
           (let ((obj (read:read stream 'eof-error #t)))
             (cond
              ((eq? *dot* obj)
	       (let ((last (read:read stream 'eof-error #t)))
		 (if (eq? '() (read:list stream char))
		     last
		     (#t (throw-error "invalid improper list" obj)))))
              (#t (cons obj (read:list stream char))))))))))

(define (read:slurp-atom stream (stop? whitespace?) (allow-eof #t)
			 (buffer #f))
  "Read until the next whitespace."
  (let ((ch (read-stream-char stream))
	(buffer (or buffer (make-string-buffer))))
    (cond
     ((eof-object? ch) (if allow-eof
			   (string-buffer->string buffer)
			   (throw-error "unexpected eof" "")))
     ((stop? ch) (string-buffer->string buffer))
     ((and allow-eof (macro-character? ch))
      (begin (unread-stream-char stream ch)
             (string-buffer->string buffer)))
     ((eq? ch #\\)
      (write-stream buffer (read:escaped stream))
      (read:slurp-atom stream
		       'stop? stop?
		       'allow-eof allow-eof
		       'buffer buffer))
     (#t
      (write-stream buffer ch)
      (read:slurp-atom stream
		       'stop? stop?
		       'allow-eof allow-eof
		       'buffer buffer)))))

(define (read:escaped stream)
  "Read an escaped character, and throw an error on EOF."
  (let ((ch (read-stream-char-safe stream)))
    (cond
     ((eq? ch #\n) #\newline)
     ((eq? ch #\t) #\tab)
     (#t ch))))

(define (read:from-token str)
  "Turn the token in the string into either an integer, real, or symbol."
  (let ((lst (string->list str)))
    (cond
     ((integer-string-list? lst *digits*) (string-list->integer lst 10))
     ((real-string-list? lst)             (string-list->real lst))
     (#t                                  (string->symbol str)))))

;; Take over for old reader
(define old-read read-port)

(define (read-stream stream)
  (read:read (ensure-pushback-stream stream)))

(define (read-port port)
  "Read an expression from a port."
  (read-stream (make <native-input-stream> 'port port)))

(define read read-port)

(define read-from-string (compose read-stream make-string-buffer)
  "Read an expression from a string.")
