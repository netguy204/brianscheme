; This is just the beginnings of pulling in the PCRE library. This
; isn't very clean yet and surely has some memory leaks at the moment.

(require 'ffi)

(let* ((pcre (ffi:dlopen "libpcre.so"))
       (compile (ffi:dlsym pcre "pcre_compile"))
       (free (ffi:dlsym pcre "pcre_free"))
       (exec (ffi:dlsym pcre "pcre_exec")))

  (define (pcre:compile pattern)
    "Compile a regexp."
    (ffi:funcall compile 'ffi-pointer
                 (ffi:string-to-alien pattern)
                 (ffi:int-to-alien 0)
                 (ffi:make-pointer-array 1)
                 (ffi:make-pointer-array 1)
                 (ffi:int-to-alien 0)))

  (define (pcre:free re)
    "Free a compiled regexp. Currently segfaults. :-("
    (ffi:funcall free 'ffi-void re))

  (define (pcre:exec re str)
    (ffi:funcall exec 'ffi-uint
                 re
                 (ffi:int-to-alien 0)
                 (ffi:string-to-alien str)
                 (ffi:int-to-alien (string-length str))
                 (ffi:int-to-alien 0)
                 (ffi:int-to-alien 0)
                 (ffi:make-pointer-array 30)
                 (ffi:int-to-alien 30)))

  (define (pcre:match regexp-string string)
    "Convenience function: return #t on match, #f otherwise."
    ;; The compiled regexp needs to be freed once that works.
    (< -1 (pcre:exec (pcre:compile regexp-string) string))))
