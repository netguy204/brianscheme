; SRFI-18: Multithreading support

(require 'pth)

(define-class <thread> (<pth:thread>)
  "Instance of an SRFI-18 thread."
  ('name))

(define-method (print-object (stream <output-stream>)
                             (thread <thread>))
  (write-stream stream "#<thread ")
  (print-object stream (slot-ref thread 'name))
  (write-stream stream ">"))

(define-method (initialize (thread <thread>) args)
  (call-next-method)
  (slot-set! thread 'name (second args)))

(define (make-thread func . name)
  (let ((thread (make <thread> func (first name))))
    (pth:suspend thread)
    thread))

(define (thread? thread)
  (memq <thread> (class-cpl (class-of thread))))

(define (thread-name thread)
  (slot-ref thread 'name))

(define current-thread pth:self)

(define (thread-start! thread)
  (pth:resume thread)
  thread)

(define thread-yield! pth:yield)

(define thread-sleep! pth:sleep)

(define (thread-join! thread timeout timeout-val)
  (pth:join thread))
