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

(let* ((pth (ffi:dlopen "libpth.so"))
       (init (ffi:dlsym pth "pth_init"))
       (spawn (ffi:dlsym pth "pth_spawn"))
       (yield (ffi:dlsym pth "pth_yield"))
       (self (ffi:dlsym pth "pth_self"))
       (join (ffi:dlsym pth "pth_join"))
       (suspend (ffi:dlsym pth "pth_suspend"))
       (resume (ffi:dlsym pth "pth_resume"))
       (sleep (ffi:dlsym pth "pth_sleep"))
       (usleep (ffi:dlsym pth "pth_usleep"))
       (kill (ffi:dlsym pth "pth_kill")))

  (define (pth:init)
    "Initialize the Pth library."
    (= 1 (ffi:funcall init 'ffi-uint)))

  (define (pth:spawn func)
    "Create a thread."
    (let* ((cif (ffi:make-function-spec 'ffi-void (list 'ffi-void)))
           (closure (ffi:create-closure (ffi:cif-cif cif)
                                        func
                                        (ffi:alien-to-int 0))))
      (ffi:funcall spawn 'ffi-pointer
                   (ffi:int-to-alien 0)
                   closure
                   (ffi:int-to-alien 0))))

  (define (pth:yield)
    "Yield to the Pth scheduler."
    (= 1 (ffi:funcall yield 'ffi-uint (ffi:int-to-alien 0))))

  (define (pth:self)
    "Return a handle for the current thread."
    (ffi:funcall join 'ffi-pointer))

  (define (pth:join pth)
    "Join the current thread with given thread."
    (= 1 (ffi:funcall join 'ffi-uint pth (ffi:int-to-alien 0))))

  (define (pth:suspend pth)
    "Suspend the given thread, current thread is not allowed."
    (= 1 (ffi:funcall suspend 'ffi-uint pth)))

  (define (pth:resume pth)
    "Resume the previously suspended thread."
    (= 1 (ffi:funcall resume 'ffi-uint pth)))

  (define (pth:sleep sec)
    "Like POSIX sleep(), but doesn't block all threads."
    (= 0 (ffi:funcall sleep 'ffi-uint (ffi:int-to-alien sec))))

  (define (pth:usleep usec)
    "Like POSIX usleep(), but doesn't block all threads."
    (= 0 (ffi:funcall usleep 'ffi-uint (ffi:int-to-alien usec))))

  (define (pth:kill)
    "Tear down the Pth library."
    (= 1 (ffi:funcall kill 'ffi-uint))))

(pth:init)
