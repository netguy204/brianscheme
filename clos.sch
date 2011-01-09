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

; DESCRIPTION:
;
; This loads the tiny-clos library created by some very bright
; people at XEROX Parc.
;
; http://community.schemewiki.org/?Tiny-CLOS
;
; Interestingly, this implementation supports a meta-object-protocol
; just like real clos. It's dog slow, but it works!

(require "clos/clos.sch")

;; syntax for building generic methods
(define-syntax (define-generic name . documentation)
  "syntax for declaring new generic functions"
  (if documentation
      (add-documentation name (car documentation)))
  `(define ,name (make-generic)))

(define-syntax (define-method name-and-specialized-args . body)
  "syntax for adding a method to a generic function"
  (let ((args (gensym)))
    `(add-method ,(first name-and-specialized-args)
       (make-method (list . ,(map second
				  (filter pair?
				    (rest name-and-specialized-args))))
         (lambda (call-next-method
		  . ,(map (lambda (v)
			    (if (pair? v)
				(first v)
				v))
			  (rest name-and-specialized-args)))
	   . ,body)))))

(define <standard-class> (make-class (list <class>)
				     (list)))

(define (initialize-slots object initargs)
  "initialize slots by keyword slot names"
  (let ((not-there (list 'shes-not-there)))
    (dolist (slot (class-slots (class-of object)))
      (let* ((name (car slot))
	     (value  (getl initargs name not-there)))
	(if (eq? value not-there)
	    'do-nothing
	    (slot-set! object name value))))))


(define-syntax (define-class name supers documentation slots)
  "creates a new class of type <standard-class> with slots initialized using keyword args and user defined supers (or <object>)"
  `(begin
     (define ,name (make <standard-class>
		     'direct-supers ,(if supers
					 `(list . ,supers)
					 `(list <object>))
		     'direct-slots (list . ,slots)
		     'class-name ',name))
     (define-method (initialize (obj ,name) args)
       (call-next-method)
       (initialize-slots obj args))))

(define-class <output-stream> ()
  "Most basic output stream abstraction.")

(define-generic write-stream
  "write something to a stream that accepts it")

;; handle null terminated strings by converting them to
;; characters and calling write-stream with that
(define-method (write-stream (stream <output-stream>)
			     (str <string>))
  (let loop ((idx 0))
    (let ((char (string-ref str idx)))
      (unless (= (char->integer char) 0)
	      (write-stream stream char)
	      (loop (+ idx 1))))))


(define-class <native-output-stream> (<output-stream>)
  "an output-stream that wraps a port"
  ('port))

;; handle character by character output
(define-method (write-stream (stream <native-output-stream>)
			     (char <char>))
  (write-char char (slot-ref stream 'port)))

;; but this stream can handle blocks too so make that
;; go fast
(define-method (write-stream (stream <native-output-stream>)
			     (str <string>))
  (display-string str (slot-ref stream 'port)))

(define stdout-stream (make <native-output-stream> 'port stdout))
(define stderr-stream (make <native-output-stream> 'port stderr))

;; these overrides on print-object provide all of the functionality of
;; the primitive writer but also give the user the opportunity to
;; define their own printed form for their classes.

(define-generic print-object
  "defines the standard written form of an object")

(define-method (print-object (strm <output-stream>)
			     (cls <class>))
  (write-stream strm #\#)
  (print-object strm (slot-ref cls 'class-name)))

(define-method (print-object (strm <output-stream>)
			     (num <number>))
  (write-stream strm (number->string num)))

(define-method (print-object (strm <output-stream>)
			     (sym <symbol>))
  (write-stream strm (symbol->string sym)))

(define-method (print-object (strm <output-stream>)
			     (pair <pair>))
  (letrec ((write-pair
	    (lambda (pair)
	      (print-object strm (car pair))
	      (cond
	       ((pair? (cdr pair))
		(write-stream strm " ")
		(write-pair (cdr pair)))
	       ((null? (cdr pair)))
	       (else
		(write-stream strm " . ")
		(print-object strm (cdr pair)))))))

    (write-stream strm "(")
    (write-pair pair)
    (write-stream strm ")")))

(define-method (print-object (strm <output-stream>)
			     (val <null>))
  (write-stream strm "nil"))

(define-method (print-object (strm <output-stream>)
			     (str <string>))
  (write-stream strm #\")
  (write-stream strm str)
  (write-stream strm #\"))

(define-method (print-object (strm <output-stream>)
			     (bool <boolean>))
  (write-stream strm #\#)
  (if bool
      (write-stream strm #\t)
      (write-stream strm #\f)))

(define-method (print-object (strm <output-stream>)
			     (vect <vector>))
  (write-stream strm "#(")
  (let loop ((idx 0))
    (when (< idx (vector-length vect))
      (if (> idx 0)
	  (write-stream strm " "))
      (print-object strm (vector-ref vect idx))
      (loop (+ idx 1))))
  (write-stream strm ")"))

(define-method (print-object (strm <output-stream>)
			     (char <char>))
  (cond
   ((eq? char #\space) (write-stream strm "#\space"))
   ((eq? char #\newline) (write-stream strm "#\newline"))
   (else (write-stream strm "#\\")
	 (write-stream strm char))))

(define-method (print-object (strm <output-stream>)
			     (prim <procedure>))
  (write-stream strm "#<procedure>"))

(define-method (print-object (strm <output-stream>)
			     (prim <input-port>))
  (write-stream strm "#<input-port>"))

(define-method (print-object (strm <output-stream>)
			     (prim <output-port>))
  (write-stream strm "#<output-port>"))

(define-class <input-stream> ()
  "most basic input stream abstraction")

(define (end-of-stream? obj)
  "predicate to detect the end of a stream"
  (eq? obj 'eos))

(define-generic read-stream
  "read a character from an input stream")

(define-generic read-stream-upto
  "read up to n characters from stream")

(define-method (read-stream-upto (strm <input-stream>)
				 (count <number>))
  (let ((result (make-string count)))
    (let loop ((idx 0))
      (if (< idx count)
	  (let ((char (read-stream strm)))
	    (if (end-of-stream? char)
		result
		(begin
		  (string-set! result idx char)
		  (loop (+ idx 1)))))
	  result))))

(define-generic read-stream-until
  "read stream until predicate is satisfied or end of stream")

(define-method (read-stream-until (strm <input-stream>)
				  (pred <procedure>))
  (let ((result (make-string-buffer)))
    (let loop ((char (read-stream strm)))
      (if (end-of-stream? char)
	  (string-buffer->string result)
	  (begin
	    (write-stream result char)
	    (if (pred char)
		(string-buffer->string result)
		(loop (read-stream strm))))))))

(define-method (read-stream-until (strm <input-stream>)
				  (char <char>))
  (read-stream-until strm
		     (lambda (ch)
		       (eq? ch char))))

(define-class <native-input-stream> (<input-stream>)
  "input stream that wraps a native port"
  ('port))

(define stdin-stream (make <native-input-stream> 'port stdin))

(define-method (read-stream (strm <native-input-stream>))
  (let ((val (read-char (slot-ref strm 'port))))
    ;; nead to translate eof
    (if (eof-object? val)
	'eos
	val)))

;; a stream buffer can be written to or read from as a stream (well,
;; it will be able to when I fix multiple inheritance in clos)
(define-class <string-buffer> (<output-stream>) ;; <input-stream>)
  "accumulates the values written to it in a string"
  ('string
   'string-length
   'storage-length))

(define (make-string-buffer . initial-value)
  "construct a new string buffer, optionally with an initial value"
  (if initial-value
      (make <string-buffer>
	'string (car initial-value)
	'string-length (string-length (car initial-value))
	'storage-length (string-length (car initial-value)))

      (let ((length 64))
	(make <string-buffer>
	  'string (make-string length)
	  'string-length 0
	  'storage-length length))))

(define (string-buffer->string buffer)
  "convert a <string-buffer> to a string"
  (slot-ref buffer 'string))

(define (%copy-into target source count)
  "private. assumes target is big enough"
  (let loop ((idx 0))
    (when (< idx count)
	  (string-set! target idx
		       (string-ref source idx))
	  (loop (+ idx 1)))
    target))

(define-method (write-stream (strm <string-buffer>)
			     (char <char>))
  ;; ensure there is sufficient storage
  (when (= (slot-ref strm 'string-length)
	   (slot-ref strm 'storage-length))

	(let* ((old-length (slot-ref strm 'string-length))
	       (new-length (* 2 old-length))
	       (new-string (make-string new-length)))

	  (slot-set! strm 'string
		     (%copy-into new-string
				 (slot-ref strm 'string)
				 old-length))
	  (slot-set! strm 'storage-length new-length)))

  ;; append the character
  (string-set!
   (slot-ref strm 'string)
   (slot-ref strm 'string-length)
   char)

  ;; increment the string size
  (slot-set! strm 'string-length
	     (+ 1 (slot-ref strm 'string-length)))

  #t)

;; HACK for now. this <input-string-buffer> business goes away
;; when multiple inheritance starts working
(define-class <input-string-buffer> (<input-stream>)
  "wrapper around a string buffer to provide an input-stream interface"
  ('buffer
   'read-index))

(define (string-buffer->input-stream buffer)
  "HACK. wrap a string-buffer so we can read it"
  (make <input-string-buffer>
    'buffer buffer
    'read-index 0))

(define-method (read-stream (instrm <input-string-buffer>)
			    (char <char>))
  (let ((strm (slot-ref instrm 'buffer)))
    (if (= (slot-ref instrm 'read-index)
	   (slot-ref strm 'string-length))
	'eos
	(let ((val (string-ref (slot-ref strm 'string)
			       (slot-ref instrm 'read-index))))
	  (slot-set! instrm 'read-index
		     (+ 1 (slot-ref instrm 'read-index)))
	  val))))

(define (string-buffer-example)
  "example of using string-buffer"
  (set! tt (make-string-buffer "hello crazy world"))
  (set! tt2 (string-buffer->input-stream tt))
  (print-object stdout-stream (read-stream-until tt2 #\space))
  (newline)
  (print-object stdout-stream (read-stream-until tt2 #\space))
  (newline)
  (print-object stdout-stream (read-stream-until tt2 #\space))
  (newline))

