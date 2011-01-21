(require 'compiler)
(require 'unittest)

(define-syntax (compiles-same exp)
  (let ((ires (gensym))
	(cres (gensym))
	(cexp (gensym)))

    `(let* ((,ires (,exp))
	    (,cexp ((compiler ',exp)))
	    (,cres (,cexp)))
       (equal? ,ires ,cres))))


(define-test (vm-test)
  (check
   ;; test the const opcode
   (compiles-same
    (lambda ()
      (letrec ((length (lambda (lst) (iter lst 0)))
	       (iter (lambda (lst n)
		       (cond
			((null? lst) n)
			(else (iter (rest lst) (+ n 1)))))))
	(length '(4 3 2 1 2 3)))))

   ;; verify atomicity of various things
   (compiles-same
    (lambda ()
      (list 1 'a null? define gensym "foo" '#(1 2 3))))

   ;; verify that closed environments are independent
   (compiles-same
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


   ;; verify that varargs works
   (compiles-same
    (lambda ()
      (letrec ((list2
		(lambda args args)))
     
	(list2 1 2 3))))


   ;; properly scoped set!
   (compiles-same
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

   ;; truthy-ness
   (compiles-same
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
	
	result)))))

(define-test (crashing-vm-test)
  (check
   ;; complex if/else
   (compiles-same
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
	      (check 5)))))))


