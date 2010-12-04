(define (display ln)
  (write-port stdout ln)
  (newline))

(define (read) (read-port stdin))
(define number? integer?)
(define true #t)
(define false #f)

(load "ch4-mceval.scm")
(define the-global-environment base-env)
#t

