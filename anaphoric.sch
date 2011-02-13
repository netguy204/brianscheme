;; DESCRIPTION: Anaphoric macros from On Lisp

(define-syntax (alambda args . body)
  `(letrec ((self (lambda ,args ,@body)))
     self))

(define-syntax (aif test then . else)
  `(let ((it ,test))
     (if it ,then ,(car else))))
