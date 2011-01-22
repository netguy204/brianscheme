; Random number generation

; NOTE: This Mersenne twister seems to be working ok, but hasn't been
; fully tested yet. I think there's an issue with it limiting itself
; to 31-bits right now, due to the way BrianScheme currently handles
; numbers.

(require 'clos)
(require 'ffi)

(define *mask-16* (- (expt 2 16) 1))
(define *mask-31* (- (expt 2 31) 1))
(define *mask-32* (logor (ash *mask-31* 1) 1))

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
		      (logand *mask-32*
                              (+ i (* 1812433253 (logxor prev
                                                         (ash prev -30))))))))
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
    (logand *mask-32* y)))

(define-generic copy
  "Copy an object.")

(define-method (copy (rng <mersenne>))
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
	   (y (+ (ash (logand (logxor *mask-32* *mask-31*)
                              (vector-ref mt i)) -31)
		 (logand *mask-31* (vector-ref mt j)))))
      (vector-set! mt i (logxor (vector-ref mt (mod (+ i 397) 624))
				(ash y -1)))
      (if (= 1 (mod y 2))
	  (vector-set! mt i (logxor (vector-ref mt i) 2567483615))))))

;; Middle-square algorithm -- don't use this seriously

(define-class <middle-square> (<random-state>)
  "The middle-square method."
  ('state))

(define-method (initialize (rng <middle-square>) args)
  (slot-set! rng 'state (or (first args) (getpid))))

(define-method (copy (rng <middle-square>))
  (make <middle-square> (slot-ref rng 'state)))

(define-method (generate (rng <middle-square>))
  (let ((state (slot-ref rng 'state))
        (lower 0)
        (upper 0))
    (set! lower (logand 65535 (ash (* state state) -8)))
    (set! upper (logand 65535 (ash (* lower lower) -8)))
    (slot-set! rng 'state (logor (ash upper 16) lower))
    (slot-ref rng 'state)))

;; Standard, non-CLOS interface

(define (make-random-state seed)
  (make <mersenne> seed))

(define (copy-random-state state)
  (let ((rng (or state *random-state*)))
    (copy rng)))

(define *random-state* (make-random-state (getpid)))

(define (random n state)
  "Generate a random number between 0 and n."
  (let* ((rng (or state *random-state*))
         (num (generate rng)))
    (if (integer? n)
	(abs (mod (generate rng) n))
	(* n 0.5 (+ (/ (logand num *mask-16*) 1.0 *mask-16*)
                    (/ (ash num -16) 1.0 *mask-16*))))))

(define *random-normal-pool* '()
  "Extra numbers generated from the pool.")

(define (random:normal state)
  "Generate number from normal distribution: Box-Muller transformation."
  (let ((rng (or state *random-state*)))
    (if *random-normal-pool* (pop! *random-normal-pool*)
        (let* ((x1 (- (random 2.0 rng) 1))
               (x2 (- (random 2.0 rng) 1))
               (w (+ (* x1 x1) (* x2 x2))))
          (if (>= w 1.0)
              (random:normal rng) ; try again
              (let ((base (sqrt (/ (* -2.0 (log w)) w))))
                (push! (* x1 base) *random-normal-pool*)
                (* x2 base)))))))

(define (random:exp state)
  "Generate a number in the exponential distribution."
  (let ((rng (or state *random-state*)))
    (- (log (random 1.0 rng)))))
