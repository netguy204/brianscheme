(require 'ffi)

(let* ((pth (ffi:dlopen "libpth.so"))
       (init (ffi:dlsym pth "pth_init"))
       (spawn (ffi:dlsym pth "pth_spawn"))
       (yield (ffi:dlsym pth "pth_yield"))
       (sleep (ffi:dlsym pth "pth_sleep"))
       (usleep (ffi:dlsym pth "pth_usleep"))
       (kill (ffi:dlsym pth "pth_kill")))

  (define (pth:init)
    "Initialize the Pth library."
    (ffi:funcall init 'ffi-uint))

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
    (ffi:funcall yield 'ffi-uint (ffi:int-to-alien 0)))

  (define (pth:sleep sec)
    "Like POSIX sleep(), but doesn't block all threads."
    (ffi:funcall sleep 'ffi-uint (ffi:int-to-alien sec)))

  (define (pth:usleep usec)
    "Like POSIX usleep(), but doesn't block all threads."
    (ffi:funcall usleep 'ffi-uint (ffi:int-to-alien usec)))

  (define (pth:kill)
    "Tear down the Pth library."
    (ffi:funcall kill 'ffi-uint)))

(pth:init)
