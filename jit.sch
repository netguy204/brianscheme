(require 'ffi)
(require 'compiler)

(with-library (libjit "libjit.so")
  (assert libjit)

  (let ((context-create (ffi:dlsym libjit "jit_context_create")))
    (assert context-create)

    (define (jit:context-create)
      (ffi:funcall context-create 'ffi-pointer))))




