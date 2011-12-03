;;; useful for testing
(define (identity2 name) (lambda (a b) (list name a b)))
(define (identity1 name) (lambda (a) (list name a)))
(define (identity* name) (lambda a (cons name a)))

;;; how to use it
;((always2 "foo") nil (identity2 'cok) (identity2 'cerr) (identity2 'eok) (identity1 'eerr))
;(set! *s* (state-from-stream (make-string-buffer "foobar") *position-zero*))

(define (run-parser p input)
  (p (state-from-stream input *position-zero*)
     (identity2 'cok) (identity1 'cerr) (identity2 'eok) (identity1 'eerr)))

(define (parse-error msg)
  (list 'parse-error msg))

(define (merge-errors list))

;;; quick interlude to define an infinitely back-trackable view of a
;;; stream that we can use to efficiently always allow backtracking
;;; without requiring that everything be in memory before we begin or
;;; requiring that fully consumed tokens remain in memory
(define-syntax (promise . body)
  "deliver a thing that can compute body at a future date"
  `(cons nil (lambda () ,@body)))

(define (deliver p)
  "deliver the result of computing the body of the promise, possibly
by evaluating body"
  (if (cdr p)
      (let ((val ((cdr p))))
	(set-car! p val)
	(set-cdr! p nil)
	val)
      (car p)))

(define-class <parser-state> ()
  "represents a 'immutable' state of a stream during parse"
  ('promise-stream
   'value
   'position))

(define (make-position row column)
  (cons row column))

(define (position-row pos)
  (car pos))

(define (position-column pos)
  (cdr pos))

(define (incr-pos pos value)
  (if (equal? value #\newline)
      (make-position (+ (position-row pos) 1)
		     1)
      (make-position (position-row pos)
		     (+ (position-column pos) 1))))

(define *position-zero* (make-position 1 1))

(define (state-from-stream stream pos)
  (let* ((value (read-stream-char stream))
	 (next (promise (state-from-stream stream (incr-pos pos value)))))
    (make <parser-state>
      'promise-stream next
      'value value
      'position pos)))

(define-generic state-current-item
  "retrieve the current item from a state")

(define-method (state-current-item (state <parser-state>))
  (slot-ref state 'value))

(define-generic state-advance-state
  "advance the parsing state beyond the current item and return that
new state (not effectively mutating the state this is called against")

(define-method (state-advance-state (state <parser-state>))
  (deliver (slot-ref state 'promise-stream)))

(define-generic state-empty?
  "is this the state past the end of the stream?")

(define-method (state-empty? (state <parser-state>))
  (end-of-stream? (slot-ref state 'value)))

;;; that's the end of the interlude... on with the combinators

(define (always x)
  "always succeed with x as a value"
  (lambda (state cok cerr eok err)
    (eok x state)))

(define (nxt p q)
  "parse p then parse q"
  (lambda (state cok cerr eok eerr)
    (let ((pcok (lambda (item state)
		  (q state cok cerr cok cerr)))
	  (peok (lambda (item state)
		  (q state cok cerr eok eerr))))
      (p state pcok cerr peok eerr))))

(define (bind p f)
  "parse p and give the results to f to produce a new parser q which
will then be parsed"
  (lambda (state cok cerr eok eerr)
    (let ((pcok (lambda (item state)
		  (let ((q (f item)))
		    (q state cok cerr cok cerr))))
	  (peok (lambda (item state)
		  (let ((q (f item)))
		    (q state cok cerr eok eerr)))))
      (p state pcok cerr peok eerr))))

(define-syntax (defparser name-and-arguments . body)
  "create a new parser and give it a global name"
  (let ((state (gensym))
	(cok (gensym))
	(cerr (gensym))
	(eok (gensym))
	(eerr (gensym)))
    `(define ,name-and-arguments
       (lambda (,state ,cok ,cerr ,eok ,eerr)
	 (let ((parser (begin ,@body)))
	   (parser ,state ,cok ,cerr ,eok ,eerr))))))

(define-syntax (>> . args)
  "a sequence of any number of parsers that must all match in order"
  (cond
   ((length=1 args) (car args))
   (else `(nxt ,(first args) (>> ,@(rest args))))))

(define-syntax (p-let bindings . body)
  (let ((var (first (first bindings)))
	(p (second (first bindings))))
    (if (length=1 bindings)
	`(bind ,p (lambda (,var) ,@body))
	`(bind ,p (lambda (,var) (p-let ,(rest bindings) ,@body))))))

(define (never)
  (lambda (state cok cerr eok eerr)
    (eerr (parse-error 'unknown))))

(define (either p q)
  (lambda (state cok cerr eok eerr)
    (let ((peerr (lambda (err-from-p)
		   (let ((qeerr (lambda (err-from-q)
				  (eerr (merge-errors err-from-p err-from-q)))))
		     (q state cok cerr eok qeerr)))))
      (p state cok cerr eok peerr))))

(define (token consume?)
  (lambda (state cok cerr eok eerr)
    (if (state-empty? state)
	(eerr (parse-error "Input is empty"))
	(let ((item (state-current-item state))
	      (next-state (state-advance-state state)))
	  (if (consume? item)
	      (cok item next-state)
	      (eerr (parse-error (list 'unexpected item))))))))

(define (many p)
  "execute a parser 0 or more times"
  (lambda (state cok cerr eok eerr)
    (letrec ((many-err (lambda (x y)
			 (throw-error "Combinator '*' applied to a parser that accepts the empty string")))
	     (pcok (lambda (coll)
		     (lambda (item state)
		       (let ((exit-cok (lambda (x)
					 (cok (reverse (cons item coll)) state))))
			 (p state (pcok (cons item coll)) cerr many-err exit-cok)))))
	     (peerr (lambda (x)
		      (eok nil state))))
      (p state (pcok nil) cerr many-err peerr))))

;; skipping times

(define (choice . parsers)
  (if (null? parsers)
      (never)
      (let ((p (first parsers)))
	(either p (apply choice (rest parsers))))))

(define (eof)
  (lambda (state cok cerr eok eerr)
    (if (state-empty? state)
	(eok nil state)
	(eerr (parse-error "expected end of input")))))

(define satisfy token)

(define (char ch)
  (satisfy [eq? _ ch]))

(define (any-char)
  (satisfy (lambda (ch) #t)))

(define (digit)
  (satisfy [digit? _]))

(define (letter)
  (satisfy [alpha? _]))

(define (between open close p)
  (p-let ((_ open)
	  (x p)
	  (_ close))
    (always x)))

(define (many1 p)
  (p-let ((x p)
	  (xs (many p)))
    (always (cons x xs))))

(defparser (positive-int)
  (p-let ((digits (many1 (digit))))
    (always (string-list->integer digits 10))))

(defparser (negative-int)
  (p-let ((num (>> (char #\-) (positive-int))))
    (always (- num))))

(defparser (whitespace)
  (many1 (choice (char #\newline)
		 (char #\space)
		 (char #\tab))))

(defparser (symbol)
  (p-let ((f (letter))
	  (r (many (either (letter)
			   (digit)))))
    (list->string (cons f r))))


(defparser (ben-integer)
  (between (char #\i) (char #\e)
	   (either
	    (positive-int)
	    (negative-int))))
