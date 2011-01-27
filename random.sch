; Random number generation

; NOTE: This Mersenne twister has been tested and works properly on
; 32-bit and 64-bit machines.

(require 'clos)
(require 'ffi)

(define *mask-16* (- (expt 2 16) 1))
(define *mask-31* (- (expt 2 31) 1))
(define *mask-32* (logor (ash *mask-31* 1) 1))

(define (random:urandom)
  "Get 32-bits from /dev/urandom."
  (with-open-file (in "/dev/urandom")
    (let ((sum 0))
      (dotimes (i 4)
        (set! sum (logxor sum (ash (char->integer (read-char in)) (* i 8)))))
      sum)))

(define (make-seed)
  "Generate a new seed."
  (reduce logxor (list (gc) (getpid)
                       (if (bound? '*random-state*)
                           (generate *random-state*)
                           0)
                       (if (file-exists? "/dev/urandom")
                           (random:urandom)))))

(define-class <random-state> ()
  "A random state for a PRNG.")

(define-class <mersenne> (<random-state>)
  "Mersenne twister, a high-quality PRNG."
  ('mt
   'index))

(define-method (initialize (rng <mersenne>) args)
  (let ((seed (car-else args (make-seed)))
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
    (set! y (logxor y (logand (ash 1318464320 1) (ash y 7))))
    (set! y (logxor y (logand (ash 2011365376 1) (ash y 15))))
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
      (if (= 1 (abs (mod y 2)))
	  (vector-set! mt i (logxor (vector-ref mt i)
                                    (logor 1 (ash 1283741807 1))))))))

;; Middle-square algorithm -- don't use this seriously

(define-class <middle-square> (<random-state>)
  "The middle-square method."
  ('state))

(define-method (initialize (rng <middle-square>) args)
  (slot-set! rng 'state (car-else arg (make-seed))))

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

(define (copy-random-state . state)
  (let ((rng (car-else state *random-state*)))
    (copy rng)))

(define *random-state* (make-random-state (make-seed)))

(define (random n . state)
  "Generate a random number between 0 and n."
  (let* ((rng (car-else state *random-state*))
         (num (generate rng)))
    (if (integer? n)
	(abs (mod (generate rng) n))
	(* n (/ (ash num -1) 1.0 *mask-31*)))))


(define (random:uniform . state)
  "Generate a number in the uniform distribution."
  (let ((rng (car-else state *random-state*)))
    (random 1.0 rng)))

;; Extra numbers generated from the pool.
(define *random-normal-extra* '())

(define (random:normal . state)
  "Generate number from the normal distribution.."
  ;; Box-Muller transformation
  (let ((rng (car-else state *random-state*)))
    (if *random-normal-extra*
        (pop! *random-normal-extra*)
        (let* ((x1 (- (* 2.0 (random:uniform rng)) 1.0))
               (x2 (- (* 2.0 (random:uniform rng)) 1.0))
               (w (+ (* x1 x1) (* x2 x2))))
          (if (>= w 1.0)
              (random:normal rng) ; try again
              (let ((base (sqrt (/ (* -2.0 (log w)) w))))
                (push! (* x1 base) *random-normal-extra*)
                (* x2 base)))))))

(define (random:exp . state)
  "Generate a number in the exponential distribution."
  (- (log (random:uniform (car-else state *random-state*)))))

(define (random:poisson m . state)
  "Generate a number from the Poisson distribution with mean M."
  (letrec ((rng (car-else state *random-state*))
	   (L (exp (- m)))
           (iter (lambda (k p) ;; Knuth's algorithm
                   (if (> p L)
                       (iter (+ k 1) (* p (random:uniform rng)))
                       (- k 1)))))
    (iter 1 1)))

(define (random:gamma a . state)
  "Generate a number from the gamma distribution, gamma(A, 1)."
  ;; Marsaglia-Tsang method
  (let* ((rng (car-else state *random-state*))
         (d (if (< a 1.0) (+ 1.0 a) a))
         (c (/ (sqrt (* 9.0 d))))
         (x (random:normal rng))
         (v (expt (+ 1.0 (* c x)) 3.0))
         (u (random:uniform rng))
         (xsq (* x x)))
    (if (or (<= v 0.0)
            (and (>= u (- 1.0 (* 0.0331 xsq xsq)))
                 (>= (log u) (+ (* xsq 0.5) (* d (- 1.0 (+ v (log v))))))))
        (random:gamma a rng)
        (* (* d v) (if (< a 1.0) (exp (/ (- (random:exp rng)) a)) 1.0)))))

;; Below here I'm just getting excessive. The gamma distribution can
;; be used to easily generate many more distributions.

(define (random:beta a b . state)
  "Generate a number from the beta distribution, beta(a, b)."
  (let* ((rng (car-else state *random-state*))
         (r1 (random:gamma a rng)))
    (/ r1 (+ r1 (random:gamma b rng)))))

(define (random:chisq df . state)
  "Generate a number from the Chi-square distribution."
  (* 2.0 (random:gamma (/ df 2.0) (car-else state *random-state*))))

(define (random:nc-chisq df L . state)
  "Generate a number from the noncentral Chi-square distribution."
  (let ((rng (car-else state *random-state*)))
    (if (= L 0.0)
        (random:chisq df rng)
        (let ((r (random:poisson (/ L 2.0) rng))
              (cum (if (> df 0) (* 2.0 (random:gamma (/ df 2.0) rng)) 0.0)))
          (+ cum (if (= r 0) 0.0 (* 2.0 (random:gamma r rng))))))))
