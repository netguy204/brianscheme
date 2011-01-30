(define (save-image file . args)
  "Save image to FILE. If given a second argument, run that on load."
  (assert-types (file string?))
  (define *after-image-start* (car-else args repl-or-script))
  (%save-image file))

(define (create-exec-image file)
  "Turn the saved image FILE into a standalone executable."
  (let ((outfile (string-append file "~")))
    (letrec ((exe (open-input-port (find-library "bschsfx")))
             (img (open-input-port file))
             (out (open-output-port outfile))
             (iter (lambda (in char)
                     (unless (eof-object? char)
                       (write-char char out)
                       (iter in (read-char in))))))
      (iter exe (read-char exe))
      (iter img (read-char img))
      (close-input-port exe)
      (close-input-port img)
      (close-output-port out)
      (chmod outfile 493)
      (rename-file outfile file))))

;; this list of hooks will be executed before the user image load
;; handler is run
(set! *load-hooks* nil)

(define (*image-start*)
  (dolist (hook *load-hooks*)
    (hook))

  (*after-image-start*))
