;; call/cc threading

(require 'clos)
(require 'queue)

(define threads:suspended '())
(define threads:waiting '())
(define threads:ready (make-queue))
(define threads:running #f)

(define thread-counter 0)

;; CLOS stuff

(define-class <thread> ()
  "A single state of execution."
  ('name 'call 'waiting 'port 'sleep 'dead))

(define-method (print-object (stream <output-stream>)
                             (thread <thread>))
  (write-stream stream "#<thread ")
  (print-object stream (slot-ref thread 'name))
  (write-stream stream ">"))

(define-method (initialize (thread <thread>) args)
  (slot-set! thread 'call (first args))
  (if (null? (second args))
      (slot-set! thread 'name (- (inc! thread-counter) 1))
      (slot-set! thread 'name (second args)))
  (slot-set! thread 'waiting #f)
  (slot-set! thread 'dead #f))

;; Scheduler

(define (unwait-thread thread)
  (set! threads:waiting (delete thread threads:waiting))
  (slot-set! thread 'waiting #f)
  (enqueue! threads:ready thread))

(define (wait-for-threads)
  (let ((reads '())
	(writes '())
	(sleeper #f))
    (dolist (thread threads:waiting)
       (let ((type (slot-ref thread 'waiting)))
	 (cond
	  ((eq? type 'sleep)
	    (if (not sleeper)
		(set! sleeper thread)
		(if (< (slot-ref thread 'sleep) (slot-ref sleeper 'sleep))
		    (set! sleeper thread))))
	  ((eq? type 'read) (push! thread reads))
	  ((eq? type 'write) (push! thread writes)))))
    (let ((read-ports  (map [slot-ref _ 'port] reads))
	  (write-ports (map [slot-ref _ 'port] writes)))
      (let ((avail (select read-ports write-ports '()
			   0 (if sleeper (slot-ref sleeper 'sleep) 0))))
	(if (equal? avail '(() () ()))
	    (unwait-thread sleeper)
	    (let ((read-map (map cons read-ports reads))
		  (write-map (map cons write-ports writes))
		  (remap (lambda (mapping lst)
			   (map (compose cdr (rcurry assoc mapping)) lst))))
	      (for-each unwait-thread (remap read-map (first avail)))
	      (for-each unwait-thread (remap write-map (second avail))))))))
  (next-thread))

(define (next-thread)
  "Run the next thread in the queue."
  (if (queue-empty? threads:ready)
      (wait-for-threads)
      (begin				; Run the next queued thread
	(set! threads:running (dequeue! threads:ready))
	((slot-ref threads:running 'call) #t))))

(define (thread-yield* fn)
  (slot-set! threads:running 'call fn)
  (if (slot-ref threads:running 'waiting)
      (push! threads:running threads:waiting)
      (enqueue! threads:ready threads:running))
  (next-thread))

(define (end-thread)
  (slot-set! threads:running 'dead #t)
  (next-thread))

;; User functions

(define (make-thread func . name)
  (let ((thread (make <thread> (lambda () (func) (end-thread))
		      (first name))))
    (push! thread threads:suspended)
    thread))

(define (thread? thread)
  (instance-of? <thread> thread))

(define (thread-name thread)
  (slot-ref thread 'name))

(define (current-thread)
  threads:running)

(define (thread-start! thread)
  (when (member? thread threads:suspended)
	(set! threads:suspended (delete thread threads:suspended))
	(enqueue! threads:ready thread))
  thread)

(define-syntax (thread-yield!)
  '(call/cc thread-yield*))

(define (thread-sleep! usec)
  (slot-set! threads:running 'sleep usec)
  (slot-set! threads:running 'waiting 'sleep)
  (thread-yield!))

(define (thread-wait-read! port)
  (slot-set! threads:running 'port port)
  (slot-set! threads:running 'waiting 'read)
  (thread-yield!))

;; Set up the main thread
(set! threads:running (make-thread #f 'main))
(set! threads:suspended '())

(define (threads:wrap-io)
  "Wrap standard I/O functions in thread yields."
  (let ((old-read-char read-char))
    (define (read-char port)
      (thread-wait-read! port)
      (old-read-char port))))
