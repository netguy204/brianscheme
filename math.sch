; Copyright 2010 Brian Taylor
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;

(define (integer->real n)
  "Cast integer to real number."
  (assert-types (n integer?))
  (%integer->real n))

;; build the proper promoting forms of the standard operators
(define (promoting-arithmatic op-integer op-real x y)
  "does standard type promotion on x and y and then calls the
appropriate operation for the types they end up as"
  (cond
   ((and (integer? x) (integer? y))
    (op-integer x y))
   ((and (real? x) (real? y))
    (op-real x y))
   ((and (integer? x) (real? y))
    (op-real (integer->real x) y))
   ((and (real? x) (integer? y))
    (op-real x (integer->real y)))
   (else (throw-error "cannot do arithmatic on " x " and " y))))

(define (+ . args)
  (reduce (lambda (x y)
	    (promoting-arithmatic %fixnum-add %real-add x y))
	  args))

(define (- . args)
  (if (length=1 args)
      (cond
       ((integer? (first args)) (%fixnum-sub 0 (first args)))
       ((real? (first args)) (%real-sub 0.0 (first args)))
       (else (throw-error "cannot negate" (first args))))
      (reduce (lambda (x y)
		(promoting-arithmatic %fixnum-sub %real-sub x y))
	      args)))

(define (* . args)
  (reduce (lambda (x y)
	    (promoting-arithmatic %fixnum-mul %real-mul x y))
	  args))

(define (< . args)
  (every-pair?
   (lambda (x y)
     (promoting-arithmatic %fixnum-less-than %real-less-than x y))
   args))

(define (> . args)
  (every-pair?
   (lambda (x y)
     (promoting-arithmatic %fixnum-greater-than %real-greater-than x y))
   args))

(define (= . args)
  (every-pair?
   (lambda (x y)
     (promoting-arithmatic %fixnum-equal %real-equal x y))
   args))

(define (<=2 a b)
  (or (< a b) (= a b)))

(define (<= . values)
  (every-pair? <=2 values))

(define (>=2 a b)
  (or (> a b) (= a b)))

(define (>= . values)
  (every-pair? >=2 values))

(define (mod . args)
  (reduce (lambda (x y)
	    (promoting-arithmatic %fixnum-mod %real-mod x y))
	  args))

(define (expt . args)
  (reduce (lambda (x y)
	    (promoting-arithmatic %fixnum-pow %real-pow x y))
	  args))

(define (/ . args)
  (if (length=1 args)
      (cond
       ((integer? (first args))
	(%real-div 1.0 (integer->real (first args))))
       ((real? (first args))
	(%real-div 1.0 (first args)))
       (else (throw-error "cannot invert" (first args))))
      (reduce (lambda (x y)
		(promoting-arithmatic %fixnum-div %real-div x y))
	      args)))

(define (abs a)
  (if (< a 0)
      (- 0 a)
      a))

(define (min0 a b)
  (if (< a b)
      a
      b))

(define (min . nums)
  (reduce min0 nums))

(define (max0 a b)
  (if (> a b)
      a
      b))

(define (max . nums)
  (reduce max0 nums))

(define *e* 2.7182818284590452354)

(define (exp x)
  (expt *e* x))

(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (- a (* b (/ a b))))))

(define (make-rat a b)
  (reduce-rat (cons a b)))

(define (numerator a)
  (car a))

(define (denominator a)
  (cdr a))

(define (reduce-rat rat)
  (let ((common (gcd (numerator rat) (denominator rat))))
    (cons (/ (numerator rat) common) (/ (denominator rat) common))))

(define (neg-rat rat)
  (make-rat (- 0 (numerator rat)) (denominator rat)))

(define (add-rat a b)
  (let ((na (numerator a))
	(nb (numerator b))
	(da (denominator a))
	(db (denominator b)))
    (make-rat (+ (* na db) (* nb da)) (* da db))))

(define (sub-rat a b)
  (add-rat a (neg-rat b)))

(define (mul-rat a b)
  (make-rat (* (numerator a) (numerator b))
	    (* (denominator a) (denominator b))))

(define (div-rat a b)
  (make-rat (* (numerator a) (denominator b))
	    (* (denominator a) (numerator b))))

(define (sqrt x)
  (if (integer? x)
      (%fixnum-sqrt x)
      (%real-sqrt x)))

(define (log x)
  "Natural log."
  (if (integer? x)
      (%fixnum-log x)
      (%real-log x)))

(define (ash value count)
  "Arithmatic shift VALUE left by COUNT bits. Negative shifts right."
  (assert-types (value integer?) (count integer?))
  (%ash value count))

(define (logor . args)
  "Bitwise or on arguments."
  (unless (every? integer? args)
    (throw-error "expecting type integer"))
  (reduce %logor args))

(define (logand . args)
  "Bitwise and on arguments."
  (unless (every? integer? args)
    (throw-error "expecting type integer"))
  (reduce %logand args))

(define (logxor . args)
  "Bitwise xor on arguments."
  (unless (every? integer? args)
    (throw-error "expecting type integer"))
  (reduce %logxor args))
