;;; hash-table.sch --- General hash table in Lisp

;; TODO:
;;  * resizing
;;    * auto-resizing
;;  * copy
;;  * actual hashing function
;;    * be able to hash weird things

(require 'clos)

(define-generic hash
  "Hash an object.")

(define-method (hash obj)
  (cond
   ((integer? obj) obj)
   ((char? obj) (char->integer obj))
   ((pair? obj) (logxor (hash (car obj)) (hash (cdr obj))))
   ((boolean? obj) (if obj 1 0))
   ((symbol? obj) (hash (symbol->string obj)))
   ((vector? obj)
    (let ((sum 0))
      (dovector (x obj)
        (set! sum (logxor sum (hash x))))
      sum))
   ((string? obj)
    (let ((sum 0))
      (dotimes (i (string-length obj))
        (set! sum (logxor sum (hash (string-ref obj i)))))
      sum))
   ((real? obj) 1) ; ???
   ((procedure? obj) 2) ; ???
   ((syntax-procedure? obj) 3) ; ???
   ((input-port? obj) 4) ; ???
   ((output-port? obj) 5) ; ???
   ((alien? obj) 6) ; ???
   (#t 0)))

(define-class <hash-table> ()
  "A generic hash table."
  ('vector
   'capacity))

(define-method (initialize (ht <hash-table>) args)
  (slot-set! ht 'vector (make-vector (car-else args 32) '())))

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
    (if (assoc key slot)
        (assoc-set! slot key value)
        (begin
          (vector-set! v idx (cons (cons key value) slot))
          value))))

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
