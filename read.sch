;;; read.sch --- a flexible BS reader

;; This reader takes over for the C reader, being much more flexible.

;; General functions

(define *whitespace-characters* (string->list " \n\t"))

(define (whitespace? ch)
  "Return #t if character is whitespace."
  (member? ch *whitespace-characters*))

(define (read-char-safe port)
  "Throw an error if read returns eof-object."
  (let ((ch (read-char port)))
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

(define-syntax (define-macro-character char-and-port . body)
  "Define-style syntax for creating reader macros."
  (let ((char (first char-and-port))
	(port (second char-and-port)))
    `(set-macro-character! ,char
			   (lambda (,port)
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
  (define-macro-character (ch port)
    (let* ((ch2 (read-char-safe port))
	   (fn (get-dispatch-macro-character ch ch2)))
      (if fn
	  (fn port)
	  (throw-error "unknown dispatch macro" ch2)))))

(define (set-dispatch-macro-character! ch1 ch2 fn)
  "Add dispatch reader macro for the given characters."
  (if (macro-character? ch1)
      (assq-set! (second *readtable*) ch1
		 (assq-set! (cdr (assq ch1 (second *readtable*))) ch2 fn))
      (throw-error "no dispatch macro" ch1)))

(define-syntax (define-dispatch-macro-character chars-and-port . body)
  "Define-style syntax for creating dispatch reader macros on #."
  (let ((ch1 (first chars-and-port))
	(ch2 (second chars-and-port))
	(port (third chars-and-port)))
    `(set-dispatch-macro-character! ,ch1 ,ch2
				    (lambda (,port)
				      ,@body))))

;; Define some reader macros

(make-dispatch-macro-character #\#)

(define-macro-character (#\' port)
  "Quote reader macro."
  (list 'quote (read:read port 'eof-error #t)))

(define-macro-character (#\` port)
  "Quasiquote reader macro."
  (list 'quasiquote (read:read port 'eof-error #t)))

(define-macro-character (#\, port)
  "Unquote reader macro."
  (let ((ch (read-char-safe port)))
    (cond
     ((eq? #\@ ch) (list 'unquotesplicing (read:read port 'eof-error #t)))
     (#t (begin (unread-char ch port)
		(list 'unquote (read:read port 'eof-error #t)))))))

(define-macro-character (#\" port)
  "String reader macro."
  (read:slurp-atom port 'stop? (lambda (ch) (eq? #\" ch))
		   'allow-eof #f))

(define-macro-character (#\; port)
  (read-line port))

(define-macro-character (#\( port)
  (read:list port #\)))

(define-macro-character (#\) port)
  (throw-error "read unexpected ')'" #\)))

(define *dot* (string->symbol "."))

(set-dispatch-macro-character! #\# #\t (always #t))
(set-dispatch-macro-character! #\# #\f (always #f))
(set-dispatch-macro-character! #\# #\! read-line)

(define-dispatch-macro-character (#\# #\( port)
  "Read in a vector."
  (apply vector (read:list port #\))))

(define-dispatch-macro-character (#\# #\\ port)
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

(define-dispatch-macro-character (#\# #\B port)
  (string->integer (read:slurp-atom port) 'base 2))
(set-dispatch-macro-character! #\# #\b (get-dispatch-macro-character #\# #\B))

(define-dispatch-macro-character (#\# #\O port)
  (string->integer (read:slurp-atom port) 'base 8))
(set-dispatch-macro-character! #\# #\o (get-dispatch-macro-character #\# #\O))

(define-dispatch-macro-character (#\# #\X port)
  (string->integer (read:slurp-atom port) 'base 16))
(set-dispatch-macro-character! #\# #\x (get-dispatch-macro-character #\# #\X))

(define-dispatch-macro-character (#\# #\: port)
  (string->uninterned-symbol (read:slurp-atom port)))

(define-dispatch-macro-character (#\# #\. port)
  (eval (read port 'eof-error #t)))

(define-dispatch-macro-character (#\# #\< port)
  "Produce an error."
  (throw-error "unreadable object" "#<...>"))

;; Read functions

(define (read:read port (eof-error #f))
  "Read an s-expression or object from the port."
  (let ((flush (read:flush-whitespace port)))
    (if (eof-object? flush)
	flush
	(let ((ch (read-char port)))
	  (cond
	   ((eof-object? ch) (if eof-error
				 (throw-error "unexpected eof" ch)
				 ch))
	   ((macro-character? ch) ((get-macro-character ch) port))
	   (#t (begin
		 (unread-char ch port)
		 (read:from-token (read:slurp-atom port)))))))))

(define (read:flush-whitespace port)
  "Eat all the whitespace, including comments, coming up in the port."
  (let ((ch (read-char port)))
    (if (eq? ch #\;)
        (begin
          (read-line port)
          (read:flush-whitespace port))
        (if (whitespace? ch)
            (read:flush-whitespace port)
            (if (eof-object? ch)
		ch
		(unread-char ch port))))))

(define (read:list port char)
  "Read a list from the given port, assuming opening char is gone."
  (if (eof-object? (read:flush-whitespace port))
      (throw-error "unexpected eof" "eof"))
  (let ((ch (read-char-safe port)))
    (cond
     ((eq? ch char) '())
     (#t (begin
           (unread-char ch port)
           (let ((obj (read:read port 'eof-error #t)))
             (cond
              ((eq? *dot* obj)
	       (let ((last (read:read port 'eof-error #t)))
		 (if (eq? '() (read:list port char))
		     last
		     (#t (throw-error "invalid improper list" obj)))))
              (#t (cons obj (read:list port char))))))))))

(define (read:make-buf (size 16))
  "Create a new string buffer."
  (list 0 size (make-string size)))

(define (read:buf-add! buffer ch)
  "Add character to buffer, expanding if needed."
  (let ((next (first buffer))
	(size (second buffer))
	(buf (third buffer)))
    (if (= next size)
	(let ((newbuf (read:make-buf 'size (* size 2))))
	  (dotimes (i size)
	    (string-set! (third newbuf) i (string-ref buf i)))
	  (read:buf-add! (cons next (cdr newbuf)) ch))
	(begin
	  (string-set! buf next ch)
	  (list (+ 1 next) size buf)))))

(define (read:buf-trim buffer)
  "Return a trimmed string of the buffer."
  (substring (third buffer) 0 (first buffer)))

(define (read:slurp-atom port (stop? whitespace?) (allow-eof #t)
			 (buffer #f))
  "Read until the next whitespace."
  (let ((ch (read-char port))
	(buffer (or buffer (read:make-buf))))
    (cond
     ((eof-object? ch) (if allow-eof
			   (read:buf-trim buffer)
			   (throw-error "unexpected eof" "")))
     ((stop? ch) (read:buf-trim buffer))
     ((and allow-eof (macro-character? ch))
      (begin (unread-char ch port)
             (read:buf-trim buffer)))
     ((eq? ch #\\)
      (read:slurp-atom port
		       'stop? stop?
		       'allow-eof allow-eof
		       'buffer (read:buf-add! buffer (read:escaped port))))
     (#t (read:slurp-atom port
			  'stop? stop?
			  'allow-eof allow-eof
			  'buffer (read:buf-add! buffer ch))))))

(define (read:escaped port)
  "Read an escaped character, and throw an error on EOF."
  (let ((ch (read-char-safe port)))
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
(define read read:read)
(define read-port read:read)
