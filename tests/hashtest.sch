(set! h (make-hashtab-eq 10))
(hashtab-set! h 'foo 3)
(hashtab-set! h 'bar 4)
(hashtab-set! h 'buz 9)
(hashtab-set! h 'sue 22)

(hashtab-ref h 'foo)
(hashtab-ref h 'bar)
(hashtab-ref h 'buz)
(hashtab-ref h 'sue)
(hashtab-ref h 'suee)
(hashtab-ref h 'fooo)


(set! tf (lambda (foo) 1))
(hashtab-set! h tf 99)
(hashtab-ref h tf)
(hashtab-ref h (lambda (foo) 1))
