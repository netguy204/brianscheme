;;; hash-table.sch --- General hash table in Lisp

;; TODO:
;;  * resizing
;;    * auto-resizing
;;  * copy
;;  * actual hashing function
;;    * be able to hash weird things
;;  * print form (readable?)

(require 'clos)

;; Hash definitions for each type

(define-generic hash
  "Hash an object.")

(define-method (hash (n <integer>))
  n)

(define-method (hash (char <char>))
  (char->integer char))

(define-method (hash (lst <pair>))
  (logxor (hash (car lst)) (hash (cdr lst))))

(define-method (hash (bool <boolean>))
  (if bool 1 0))

(define-method (hash (sym <symbol>))
  (hash (symbol->string obj)))

(define-method (hash (vec <vector>))
  (let ((sum 0))
    (dovector (x vec)
      (set! sum (logxor sum (hash x))))
    sum))

(define-method (hash (str <string>))
  (let ((sum 0))
    (dotimes (i (string-length str))
      (set! sum (logxor sum (hash (string-ref str i)))))
    sum))

;; TODO
(define-method (hash (x <real>))
  (round x))

;; TODO
(define-method (hash (proc <procedure>))
  1)

;; TODO
(define-method (hash (proc <syntax-procedure>))
  2)

;; TODO
(define-method (hash (port <input-port>))
  3)

;; TODO
(define-method (hash (port <output-port>))
  4)

;; TODO
(define-method (hash (port <alien>))
  5)

;; Default to slot 0
(define-method (hash obj)
  0)

;; Table implementation

(define-class <hash-table> ()
  "A generic hash table."
  ('vector
   'capacity))

(define-method (initialize (ht <hash-table>) args)
  (slot-set! ht 'vector (make-vector (car-else args 32) '()))
  (slot-set! ht 'capacity 0))

(define-generic get
  "Get a value from the object.")

(define-generic put
  "Store a value in the object.")

(define-method (get (ht <hash-table>) key)
  (let* ((v (slot-ref ht 'vector))
         (idx (mod (hash key) (vector-length v)))
         (slot (vector-ref v idx))
         (item (assoc key slot)))
    (if item
        (cdr item)
        #f)))

(define-method (put (ht <hash-table>) key value)
  (let* ((v (slot-ref ht 'vector))
         (idx (mod (hash key) (vector-length v)))
         (slot (vector-ref v idx)))
    (slot-set! ht 'capacity (+ 1 (slot-ref ht 'capacity)))
    (if (assoc key slot)
        (assoc-set! slot key value)
        (begin
          (vector-set! v idx (cons (cons key value) slot))
          value))))

;; Non-CLOS functions

(define (make-hash-table . size)
  (if size
      (make <hash-table> (car size))
      (make <hash-table>)))

(define (hash-ref ht key)
  "Get value from hash table."
  (get ht key))

(define (hash-set! ht key value)
  "Store value into hash table at key."
  (put ht key value))
