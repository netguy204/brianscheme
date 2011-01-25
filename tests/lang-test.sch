(require 'unittest)

;; macros close over their lexical environment but the compiler makes
;; expansion decisions on a form by form basis. This macro wouldn't be
;; expanded properly if we just inlined it in the check
(let ((x 1))
  (define-syntax (macmac val)
    (set! x (+ x 1))
    `(+ ,x ,val)))

(define-test (lang-test)
  (check
   ;; verify some basic closure behavior
   (= 3 (((lambda (x) (lambda (y) (+ y x))) 1) 2))
   (= 1 (let* ((x 1) (x x) (x x) (x x)) x))

   ;; macros close over their lexical environment too
   (= 3 (macmac 1))
   (= 5 (macmac 2)))

  ;; verify some interesting macros

  ;; generate some fibonacci numbers
  (check
   (equal?
    '(1 2 3 5 8)
    (do ((x 1 y)
	 (y 1 (+ x y))
	 (result nil (cons y result)))

	((= (length result) 5) (reverse result)))))


  ;; make a handy mutating function
  (letrec ((val 0)
	   (fn (lambda () (inc! val) (list val val))))

    ;; logical operators should short circuit in the usual way
    (and (fn) (fn) (fn)) ;; should evaluate fn 3 times
    (check (equal? '(4 4) (fn)))

    (or (fn) (fn) (fn)) ;; should evaluate fn once
    (check (equal? '(6 6) (fn)))

    ;; destructuring let should evaluate its bindings only once
    (let (((a b) (fn))
	  ((c d) (fn)))
      (check
       (= a 7) (= b 7)
       (= c 8) (= d 8)))))





