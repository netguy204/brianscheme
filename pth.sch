; Copyright 2010 Christopher Wellons
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

; DESCRIPTION: Imports functions from GNU Pth (portable threads) using
; FFI. Provides routines for non-preemptive threading. See the GNU Pth
; manual for details.

; WARNING: Aborting threads, such as with pth:kill, can cause memory
; leaks because the threads will never pop their GC roots. As with any
; threading library, try not to abort your threads.

(require 'ffi)
(require 'clos)

(with-library (pth "libpth.so")
  (let ((init (ffi:dlsym pth "pth_init"))
	(spawn (ffi:dlsym pth "pth_spawn"))
	(yield (ffi:dlsym pth "pth_yield"))
	(self (ffi:dlsym pth "pth_self"))
	(join (ffi:dlsym pth "pth_join"))
	(suspend (ffi:dlsym pth "pth_suspend"))
	(resume (ffi:dlsym pth "pth_resume"))
	(sleep (ffi:dlsym pth "pth_sleep"))
	(usleep (ffi:dlsym pth "pth_usleep"))
	(event (ffi:dlsym pth "pth_event"))
	(wait (ffi:dlsym pth "pth_wait"))
	(kill (ffi:dlsym pth "pth_kill")))

    ;; Define wrapper classes to hide Pth's alien nature

    (define-class <pth:thread> ()
      "Instance of a Pth thread."
      ('alien-thread
       'thread-fn))

    (define-method (print-object (strm <output-stream>)
				 (thr <pth:thread>))
      (let ((address (ffi:address-of (slot-ref thr 'alien-thread))))
	(write-stream strm "#<pth:thread ")
	(write-stream strm (number->string (ffi:alien-to-int address)))
	(write-stream strm ">")))

    (let ((threads nil))
      (define-method (initialize (thread <pth:thread>) args)
	(let* ((func (first args))
	       (cif (ffi:make-function-spec 'ffi-void (list 'ffi-void)))
	       (closure (ffi:create-closure (ffi:cif-cif-ref cif)
					    func
					    (ffi:int-to-alien 0)))
	       (thr (ffi:funcall spawn 'ffi-pointer
				 (ffi:int-to-alien 0)
				 closure
				 (ffi:int-to-alien 0))))
	  (slot-set! thread 'alien-thread thr)
	  (slot-set! thread 'thread-fn func)
	  (push! thread threads))))

    (define-class <pth:event> ()
      "Instance of a Pth event."
      ('alien-event))

    (define-method (print-object (strm <output-stream>)
				 (event <pth:event>))
      (write-stream strm "#<pth:event>"))

    ;; Create bindings to Pth

    (define (pth:init)
      "Initialize the Pth library."
      (= 1 (ffi:funcall init 'ffi-uint)))

    (define (pth:spawn func)
      "Create a thread."
      (make <pth:thread> (lambda (alien-arg-array) (func))))

    (define (pth:yield)
      "Yield to the Pth scheduler."
      (= 1 (ffi:funcall yield 'ffi-uint (ffi:int-to-alien 0))))

    (define (pth:self)
      "Return a handle for the current thread."
      (ffi:funcall join 'ffi-pointer))

    (define (pth:join pth)
      "Join the current thread with given thread."
      (= 1 (ffi:funcall join 'ffi-uint
			(slot-ref pth 'alien-thread)
			(ffi:int-to-alien 0))))

    (define (pth:suspend pth)
      "Suspend the given thread, current thread is not allowed."
      (= 1 (ffi:funcall suspend 'ffi-uint (slot-ref pth 'alien-thread))))

    (define (pth:resume pth)
      "Resume the previously suspended thread."
      (= 1 (ffi:funcall resume 'ffi-uint (slot-ref pth 'alien-thread))))

    (define (pth:sleep sec)
      "Like POSIX sleep(), but doesn't block all threads."
      (= 0 (ffi:funcall sleep 'ffi-uint (ffi:int-to-alien sec))))

    (define (pth:usleep usec)
      "Like POSIX usleep(), but doesn't block all threads."
      (= 0 (ffi:funcall usleep 'ffi-uint (ffi:int-to-alien usec))))

    (define (pth:event type handle)
      "Create a new Pth event."
      (make <pth:event>
	'alien-event (ffi:funcall event 'ffi-pointer
				  (ffi:int-to-alien type)
				  (ffi:int-to-alien handle))))

    (define (pth:wait event)
      "Wait on the given Pth event."
      (ffi:funcall wait 'ffi-uint (slot-ref event 'alien-event)))

    (define (pth:kill)
      "Tear down the Pth library."
      (= 1 (ffi:funcall kill 'ffi-uint)))

    (pth:init)

    ;; predefined events
    (define stdin-event (pth:event 4098 0))))


; event subject classes
(define nc:event-fd 2)
(define nc:event-select 4)
(define nc:event-sigs 8)
(define nc:event-time 16)
(define nc:event-msg 32)
(define nc:event-mutex 64)
(define nc:event-cond 128)
(define nc:event-tid 256)
(define nc:event-func 512)

; event occurange restrictions
(define nc:until-fd-readable 4096)
(define nc:until-fd-writeable 8192)

(define-syntax (on-event event . body)
  "Run body after event has occurred."
  `(begin (pth:wait ,event) . ,body))

(define (pth:getch)
  "For use with ncurses, a thread-friendly getch."
  (on-event stdin-event
    (nc:getch)))
