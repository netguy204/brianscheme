;;; Input/Output functions and macros

(define-syntax (with-open-file binding . body)
  (let ((var (first binding))
        (file (second binding))
        (res (gensym)))
    `(let ((,var (open-input-port ,file)))
       (if (eof-object? ,var)
           (throw-error "failed to open" ,file)
           (let ((,res (begin ,@body)))
             (close-input-port ,var)
             ,res)))))

(define (chmod file mode)
  "Change mode of filename in string FILE to MODE."
  (assert-types (file string?) (mode integer?))
  (%chmod file mode))

(define (rename-file oldname newname)
  "Rename/move a file."
  (assert-types (oldname string?) (newname string?))
  (%rename-file oldname newname))
