(require 'compiler)

(define-syntax (assert-compiles-same exp)
  (let ((ires (gensym))
	(cres (gensym))
	(cexp (gensym)))

    `(let* ((,ires (,exp))
	    (,cexp ((compiler ',exp)))
	    (,cres (,cexp)))
       (unless (equal? ,ires ,cres)
	       (for-each display
			 (list ,ires "!=" ,cres))
	       (newline)
	       (display ',exp)
	       (newline)

	       (show-fn ,cexp 0)
	       (throw-exit "test failed"))

       (list ,ires "=" ,cres))))

; test the const opcode
(assert-compiles-same
 (lambda ()
   (letrec ((length (lambda (lst) (iter lst 0)))
	    (iter (lambda (lst n)
		    (cond
		     ((null? lst) n)
		     (else (iter (rest lst) (+ n 1)))))))
     (length '(4 3 2 1 2 3)))))


; verify atomicity of various things
(assert-compiles-same
 (lambda ()
   (list 1 'a null? define gensym "foo" '#(1 2 3))))


; verify that closed environments are independent
(assert-compiles-same
 (lambda ()
   (letrec ((make-counter
	     (lambda ()
	       (let ((val 0))
		 (lambda ()
		   (inc! val)
		   val)))))

     (let ((c1 (make-counter))
	   (c2 (make-counter)))

       ;; expect (1 1 2 3 2 3)
       (list (c1) (c2) (c1) (c1) (c2) (c2))))))


; verify that varargs works
(assert-compiles-same
 (lambda ()
   (letrec ((list2
	     (lambda args args)))
     
     (list2 1 2 3))))


; properly scoped set!
(assert-compiles-same
 (lambda ()
   (let ((result nil))
     (set! test-global 'foo)
     (push! test-global result)
     (let ((test-global 'bar))
       (push! test-global result)
       (set! test-global 'buz)
       (push! test-global result))
     (push! test-global result)
     result)))

; truthy-ness
(assert-compiles-same
 (lambda ()
   (let ((result nil)
	 (truth #t)
	 (lie #f))
     (if #t
	 (push! 'a result))
     (if #f
	 (push! 'b result))
     (if (cons 1 2)
	 (push! 'c result))
     (if nil
	 (push! 'd result))
     (if '()
	 (push! 'e result))
     (if truth
	 (push! 'f result))
     (if lie
	 (push! 'g result))

     result)))

; complex if/else
(assert-compiles-same
 (lambda ()
   (letrec ((check
	     (lambda (n)
	       (cons 'foo
		     (case n
		       (0 1)
		       (1 2)
		       (2 3)
		       (3 4)
		       ('a 'b)
		       ('b 'c)
		       ('c 'd)
		       (else 'q))))))

     (list (check 1)
	   (check 2)
	   (check 3)
	   (check 4)
	   (check 'a)
	   (check 'b)
	   (check 'c)
	   (check 5)))))



'tests-finished
