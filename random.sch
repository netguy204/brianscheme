; Random number generation

; NOTE: This Mersenne twister seems to be working ok, but hasn't been
; fully tested yet. I think there's an issue with it limiting itself
; to 31-bits right now, due to the way BrianScheme currently handles
; numbers.

(require 'clos)

(define-class <random-state> ()
  "A random state for a PRNG.")

(define-class <mersenne> (<random-state>)
  "Mersenne twister, a high-quality PRNG."
  ('mt
   'index))

(define-method (initialize (rng <mersenne>) args)
  (let ((seed (first args))
	(mt (make-vector 624 0)))
    (vector-set! mt 0 seed)
    (dotimes (j 623)
       (let ((prev (vector-ref mt j))
	     (i (+ j 1)))
	 (vector-set! mt i
		      (+ i (* 1812433253 (logxor prev (ash prev -30)))))))
    (slot-set! rng 'mt mt))
  (slot-set! rng 'index 0))

(define-generic generate
  "Generate a random number.")

(define-method (generate (rng <mersenne>))
  (if (= 0 (slot-ref rng 'index))
      (regenerate rng))
  (let ((y (vector-ref (slot-ref rng 'mt) (slot-ref rng 'index))))
    (set! y (logxor y (ash y -11)))
    (set! y (logxor y (logand 2636928640 (ash y 7))))
    (set! y (logxor y (logand 4022730752 (ash y 15))))
    (set! y (logxor y (ash y -18)))
    (slot-set! rng 'index (mod (+ 1 (slot-ref rng 'index)) 624))
    y))

(define-generic copy
  "Copy an object.")

(define-method (copy rng <mersenne>)
  (let* ((new-rng (make <mersenne> 0))
	 (mt (slot-ref rng 'mt))
	 (new-mt (slot-ref new-rng 'mt)))
    (slot-set! new-rng 'index (slot-ref rng 'index))
    (dotimes (i 624)
      (vector-set! new-mt i (vector-ref mt i)))
    new-rng))

(define-generic regenerate
  "Regenerate the Mersenne state vector.")

(define-method (regenerate (rng <mersenne>))
  (dotimes (i 624)
    (let* ((mt (slot-ref rng 'mt))
	   (j (mod (+ i 1) 624))
	   (y (+ (ash (logand 2147483648 (vector-ref mt i)) -30)
		 (logand 2147483647 (vector-ref mt j)))))
      (vector-set! mt i (logxor (vector-ref mt (mod (+ i 397) 624))
				(ash y -1)))
      (if (= 1 (mod y 2))
	  (vector-set! mt i (logxor (vector-ref mt i) 2567483615))))))

;; Standard, non-CLOS interface

(define (make-random-state seed)
  (make <mersenne> seed))

(define (copy-random-state state)
  (let ((rng (or state *random-state*)))
    (copy rng)))

(define *random-state* (make-random-state 8))

(define (random n state)
  "Generate a random number between 0 and n."
  (let ((rng (or state *random-state*)))
    (if (integer? n)
	(mod (generate rng) n)
	(* n (/ (generate rng) (expt 2.0 31))))))
