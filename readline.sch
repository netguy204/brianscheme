(require 'ffi)

(with-library (rl "libreadline")
  (let ((ffi-readline (ffi:dlsym rl "readline"))
        (ffi-add-history (ffi:dlsym rl "add_history")))

    (define (readline prompt)
      (let ((astr (ffi:funcall ffi-readline 'ffi-pointer prompt)))
        (if (= (ffi:alien-to-int astr) 0)
            ""
            (let ((str (ffi:alien-to-string astr)))
              (ffi:funcall ffi-add-history 'ffi-void astr)
              (ffi:free astr)
              str))))))

(define (readline-repl)
  "Provide a REPL with readline."
  (display (eval (read-string (readline "bs+> "))))
  (readline-repl))
