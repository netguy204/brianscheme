(require 'unittest)

(define-test (basic-arith)
  (check
   ;; unary arith special cases
   (= 2 (+ 2))
   (= -1 (- 1))
   (= 5 (* 5))
   (= 0.5 (/ 2))
   
   ;; unary real numbers
   (= -2.0 (- 2.0))
   (= 2.0 (+ 2.0))
   (= 2.0 (* 2.0))
   (= 0.5 (/ 1.0))

   ;; binary fixnums
   (= 5 (+ 2 3))
   (= -4 (- 1 5))
   (= 10 (* 2 5))
   (= 2 (/ 4 2))

   ;; binary reals
   (= 3.0 (+ 1.0 2.0))
   (= 1.0 (- 2.0 1.0))
   (= 4.0 (* 2.0 2.0))
   (= 2.0 (/ 4.0 2.0))

   ;; basic promotion
   (integer? (+ 1 2 3))
   (real? (+ 1 2.0 3))

   ;; bitwise logical
   (= 0 (logand 1 2))
   (= 1 (logand 1 3))
   (= 3 (logor 1 2))
   (= 0 (logxor 1 1))
   (= 2 (logxor 3 1))

   ;; other functions
   (= 8 (expt 2 3))
   (= 8.0 (expt 2.0 3))))
