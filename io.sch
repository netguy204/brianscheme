;;; Input/Output functions and macros

(define-syntax (with-open-file binding . body)
  (let ((var (first binding))
        (file (second binding))
        (res (gensym)))
    `(let ((,var (open-input-port ,file)))
       (if (eof-object? ,var)
           (throw-error "failed to open" ,file)
           (let ((,res ,@body))
             (close-input-port ,var)
             ,res)))))
