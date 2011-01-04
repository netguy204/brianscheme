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

(define (abs a)
  (if (< a 0)
      (- 0 a)
      a))

(define (min a b)
  (if (< a b)
      a
      b))

(define (max a b)
  (if (> a b)
      a
      b))

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

