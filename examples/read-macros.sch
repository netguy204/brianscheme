;; Here are some reader macro examples to get you started. See also
;; the sugar library sugar.sch, (require 'sugar).

(require 'read)

;; This is the classic example that produces a range of characters
;; using brackets. #[2 7] => (2 3 4 5 6 7)

(define-macro-character (#\] port)
  (throw-error "read unexpected ']'" #\]))

(define-dispatch-macro-character (#\# #\[ port)
  (let* ((range (read:list port #\]))
	 (min (first range))
	 (max (second range))
	 (lst '()))
    (dotimes (i (- max min -1))
      (push! (- max i) lst))
    (list 'quote lst)))

(display "#[2 7] => ")
(display #[2 7])
(newline)

;; Define an alternate dispatch character $. $u produces an ascending
;; upto list, $r produces a descending upto list, and $(...) sorts its
;; list at read-time.

(make-dispatch-macro-character #\$)

(define-dispatch-macro-character (#\$ #\u port)
  (list 'quote (upto (read:read port 'eof-error #t))))

(define-dispatch-macro-character (#\$ #\r port)
  (list 'quote (reverse (upto (read:read port 'eof-error #t)))))

(define-dispatch-macro-character (#\$ #\( port)
  (list 'quote (sort (read:list port #\)) <)))

(display "$u7 => ")
(display $u7)
(newline)

(display "$r7 => ")
(display $r7)
(newline)

(display "$(...) => ")
(display $(4 3 6 7 1 0 2))
(newline)
