(require 'math)

(define (string-append . args)
  "join a series of strings"
  (reduce prim-concat args))

(define (string-length str)
  "find the length of a string, not including null"
  (let ((len 0))
    (while (not (= (char->integer (string-ref str len))
		   0))
      (inc! len))
    len))

(define (substring str start . end)
  "Return given substring from start (inclusive) to end (exclusive)."
  (let* ((strlen (string-length str))
	 (end (car-else end strlen))
	 (len (- end start)))
    (if (or (> len strlen) (> end strlen) (> start strlen))
	(throw-error "out of string bounds" str start end))
    (if (> start end)
	(throw-error "invalid substring" start end))
    (let ((substr (make-string len #\.)))
      (dotimes (i len)
        (string-set! substr i (string-ref str (+ i start))))
      substr)))
