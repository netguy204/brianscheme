(set! define defun)

(define mapcar (fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst)) (mapcar fn (cdr lst)))))

