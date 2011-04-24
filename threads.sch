;; call/cc threading

(require 'clos)
(require 'queue)

(define threads:suspended '())
(define threads:waiting '())
(define threads:running (make-queue))
(define threads:current #f)

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
  (enqueue! threads:running thread))

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
			   0 (slot-ref sleeper 'sleep))))
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
  (if (queue-empty? threads:running)
      (wait-for-threads)
      (begin				; Run the next queued thread
	(set! threads:current (dequeue! threads:running))
	((slot-ref threads:current 'call) #t))))

(define (thread-yield* fn)
  (slot-set! threads:current 'call fn)
  (if (slot-ref threads:current 'waiting)
      (push! threads:current threads:waiting)
      (enqueue! threads:running threads:current))
  (next-thread))

(define (end-thread)
  (slot-set! threads:current 'dead #t)
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
  threads:current)

(define (thread-start! thread)
  (when (member? thread threads:suspended)
	(set! threads:suspended (delete thread threads:suspended))
	(enqueue! threads:running thread))
  thread)

(define-syntax (thread-yield!)
  '(call/cc thread-yield*))

(define (thread-sleep! usec)
  (slot-set! threads:current 'sleep usec)
  (slot-set! threads:current 'waiting 'sleep)
  (thread-yield!))

;; Set up the main thread
(set! threads:current (make-thread #f 'main))
(set! threads:suspended '())
