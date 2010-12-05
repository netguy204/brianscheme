;; Stage 3, Bytecode generation
;;
;; This is essentially a direct translation of the bytecode compiler
;; presented in Peter Norvig's "Paradigms of Artificial Intelligence
;; Programming." The bytecode should be identitcal to the bytecode
;; produced in the book.
;;
;; Eventually this bytecode will be executed on a virtual machine back
;; in primitive space.

;; Comment out the second form for loads of function trace
;; information. I should really write a real trace macro at some
;; point.

(define-syntax write-dbg (&rest args)
  `(write . ,args))

(define-syntax write-dbg (&rest args)
  #t)

(define (comp x env val? more?)
  (write-dbg 'comp x 'val? val? 'more? more?)
  (cond
   ;; expand anything with a macro at the head
   ((and (pair? x)
	 (sym-is-syntax? (car x))) (comp (comp-macroexpand0 x) env
					 val? more?))
   ((member? x '(#t #f)) (comp-const x val? more?))
   ((symbol? x) (comp-var x env val? more?))
   ((atom? x) (comp-const x val? more?))
   (else (case (first x)
	   (quote (arg-count x 1 1)
		  (comp-const (second x) val? more?))
	   (begin (comp-begin (rest x) env val? more?))
	   (set! (arg-count x 2 2)
		 (seq (comp (third x) env #t #t)
		      (gen-set (second x) env)
		      (when (not val?) (gen 'pop))
		      (unless more? (gen 'return))))
	   (if (arg-count x 2 3)
	       (comp-if (second x) (third x) (fourth x)
			env val? more?))
	   (lambda (when val?
			 (let ((f (comp-lambda (second x)
					       (rest (rest x)) env)))
			   (seq (gen 'fn f)
				(unless more? (gen 'return))))))
	   ;; generate an invocation
	   (else (comp-funcall (first x) (rest x)
			       env val? more?))))))

(define (arg-count form min max)
  (let ((n-args (length (rest form))))
    (unless (<= min n-args max)
	    (throw-error "wrong number of args"
			 form
			 "expected between"
			 min "and" max))))

(define (comp-begin exps env val? more?)
  (write-dbg 'comp-begin exps 'val? val? 'more? more?)
  (cond ((null? exps) (comp-const nil val? more?))
	((length=1 exps) (comp (first exps) env val? more?))
	(else (seq (comp (first exps) env #f #t)
		   (comp-begin (rest exps) env val? more?)))))

(define (comp-list exps env)
  (write-dbg 'comp-list exps)
  (if (null? exps) nil
      (seq (comp (first exps) env #t #t)
	   (comp-list (rest exps) env))))

(define (bytecode-literal? x)
  (or (integer? x) (boolean? x)))

(define (comp-const x val? more?)
  (write-dbg 'comp-const x 'val? val? 'more? more?)
  (when val? (seq (if (bytecode-literal? x)
		      (gen x)
		      (gen 'const x))
		  (unless more? (gen 'return)))))

(define (comp-var x env val? more?)
  (write-dbg 'comp-var x 'val? val? 'more? more?)
  (when val? (seq (gen-var x env)
		  (unless more? (gen 'return)))))

(define (false? exp)
  (or (null? exp) (eq? exp #f)))

(define (true? exp)
  (and (atom? exp) (not (false? exp))))

(define (comp-if pred then else env val? more?)
  (write-dbg 'comp-if pred 'then then 'else else
	     'val? val? 'more? more?)
  (cond
   ((false? pred)
    (comp else env val? more?))
   ((true? pred)
    (comp then env val? more?))
   (else (let ((pcode (comp pred env #t #t))
	       (tcode (comp then env val? more?))
	       (ecode (comp else env val? more?)))
	   (cond
	    ((equal? tcode ecode)
	     (seq (comp pred env #f #t)
		  ecode))
	    ((null? tcode)
	     (let ((l2 (gen-label)))
	       (seq pcode
		    (gen 'tjump l2) ecode (list l2)
		    (unless more? (gen 'return)))))
	    ((null? ecode)
	     (let ((l1 (gen-label)))
	       (seq pcode
		    (gen 'fjump l1) tcode (list l1)
		    (unless more? (gen 'return)))))
	    (else
	     (let ((l1 (gen-label))
		   (l2 (when more? (gen-label))))
	       (seq pcode (gen 'fjump l1) tcode
		    (when more? (gen 'jump l2))
		    (list l1) ecode
		    (when more? (list l2))))))))))
		    
(define (comp-funcall f args env val? more?)
  (write-dbg 'comp-funcall f 'args args
	     'val? val? 'more? more?)
  (let ((prim (primitive? f env (length args))))
    (cond
     (prim
      (if (and (not val?) (not (prim-side-effects? prim)))
	  (comp-begin args env #f more?)
	  (seq (comp-list args env)
	       (gen (prim-opcode prim))
	       (unless val? (gen 'pop))
	       (unless more? (gen 'return)))))
     ((and (starts-with f 'lambda) (null? (second f)))
      (unless (null? args) (throw-error "too many arguments"))
      (comp-begin (cdr (cdr f)) env val? more?))
     (more?
      (let ((k (gen-label 'k)))
	(seq (gen 'save k)
	     (comp-list args env)
	     (comp f env #t #t)
	     (gen 'callj (length args))
	     (list k)
	     (unless val? (gen 'pop)))))
     (else
      (seq (comp-list args env)
	   (comp f env #t #t)
	   (gen 'callj (length args)))))))

(define (primitive-procedure? obj)
  (and (procedure? obj)
       (not (compound-procedure? obj))))

(define (environment-names &rest env)
  (cond
   ((or (null? env) 
	(primitive-procedure? (car env)))
    (cdr (cdr (map first (current-environment)))))
   ((compound-procedure? (car env))
    (map first (compound-environment (car env))))
   (else nil)))

(define (compound->lambda comp)
  `(lambda ,(compound-args comp)
      ,(compound-body comp)))

(define (sym-is-syntax? sym)
  (let ((val (find-variable sym #t)))
    (if (null? val)
	#f
	(syntax-procedure? (car val)))))

(define (comp-macroexpand0 exp)
  (eval `(macroexpand0 '(,(car exp) . ,(cdr exp)))))

;; Since we don't have defstruct we fake it with a bunch of hand
;; written accessors
(define (make-fn code env name args)
  (write-dbg 'make-fn 'code code)
  (list 'fn code env name args))

(define (fn? fn)
  (starts-with fn 'fn))

(define (fn-code fn)
  (first (cdr fn)))

(define (fn-env fn)
  (second (cdr fn)))

(define (fn-name fn)
  (third (cdr fn)))

(define (fn-args fn)
  (fourth (cdr fn)))

(define (fn-bytecode fn)
  (fifth (cdr fn)))

(define (make-prim symbol n-args opcode always? side-effects?)
  (list 'prim symbol n-args opcode always? side-effects?))

(define (prim? obj)
  (starts-with obj 'prim))

(define (prim-symbol prim)
  (first (rest prim)))

(define (prim-n-args prim)
  (second (rest prim)))

(define (prim-opcode prim)
  (third (rest prim)))

(define (prim-always? prim)
  (fourth (rest prim)))

(define (prim-side-effects? prim)
  (fifth (rest prim)))

(define *primitive-fns*
  (map (lambda (fn) (cons 'prim fn))
       '((+ 2 + #t #f) (- 2 - #t #f) (* 2 * #t #f) (/ 2 / #t #f)
	 (< 2 < #f #f) (> 2 > #f #f)
	 (= 2 = #f #f) (eq? 2 eq? #f #f)
	 (car 1 car #f #f) (cdr 1 cdr #f #f) (cons 2 cons #f #f)
	 (null? 1 null? #f #f))))

;; f is primitive if it's in the table and not shadowed in the
;; environment and has the right number of arguments.
(define (primitive? f env n-args)
  (write-dbg 'primitive? f 'env env 'n-args n-args)
  (and (not (in-env? f env))
       (find (lambda (p)
	       (and (eq? f (prim-symbol p))
		    (= n-args (prim-n-args p))))
	     *primitive-fns*)))

(define (assert-symbols lst)
  (unless (or (null? lst)
	      (and (pair? lst)
		   (every? symbol? lst)))
	  (error "lambda arglist must be list of symbols"
		 lst)
	  (exit 1)))

(define (comp-lambda args body env)
  (write-dbg 'comp-lambda args 'body body)
  (new-fun (seq (gen-args args 0)
		(comp-begin body
			    (cons (make-true-list args) env)
			    #t #f))
	   env "unknown" args))

(define (gen-args args n-so-far)
  (cond
   ((null? args) (gen 'args n-so-far))
   ((symbol? args) (gen 'args. n-so-far))
   ((and (pair? args)
	 (symbol? (first args)))
    (gen-args (rest args) (+ n-so-far 1)))
   (else (throw-error "illegal argument list" args))))

(define (make-true-list dotted-list)
  (cond
   ((null? dotted-list) nil)
   ((atom? dotted-list) (list dotted-list))
   (else (cons (first dotted-list)
	       (make-true-list (rest dotted-list))))))

(define (new-fun code env name args)
  (assemble (make-fn (optimize code) env name args)))

(define (optimize code) code)

(define (assemble fn) fn)
	       
(define label-num 0)

(define (compiler x)
  (set! label-num 0)
  (comp-lambda nil (list x) nil))

(define (gen opcode &rest args)
  (write-dbg 'gen opcode 'args args)
  (list (cons opcode args)))

(define (seq &rest code)
  (append-all code))

(define (string obj)
  (cond
   ((string? obj) obj)
   ((symbol? obj) (symbol->string obj))
   (else (throw-error "can't make" obj "a string"))))

(define (gen-label &rest opt)
  (let ((prefix (if (pair? opt)
		    (string (car opt))
		    "L")))
    (write-dbg 'gen-label prefix)
    (set! label-num (+ label-num 1))
    (string->symbol (concat prefix (number->string label-num)))))
  
(define (gen-var var env)
  (write-dbg 'gen-var var)
  (let ((p (in-env? var env)))
    (if (not (null? p))
	(gen 'lvar (first p) (second p) ";" var)
	(gen 'gvar var))))

(define (gen-set var env)
  (write-dbg 'gen-set var)
  (let ((p (in-env? var env)))
    (if p
	(gen 'lset (first p) (second p) ";" var)
	(if (assoc var *primitive-fns*)
	    (throw-error "can't alter the constant" +)
	    (gen 'gset var)))))

(define (in-env? symbol env)
  (let ((frame (find (lambda (f) (member? symbol f)) env)))
    (if (null? frame)
	nil
	(list (index-eq frame env) (index-eq symbol frame)))))

(define (make-space spaces)
  (reduce concat (duplicate " " spaces) ""))

(define (show-fn fn indent)
  (if (not (fn? fn))
      (write "#  " fn)
      (begin
	(newline)
	(dolist (instr (fn-code fn))
		(if (symbol? instr)
		    (write instr ":")
		    (dolist (arg instr)
			    (show-fn arg (+ indent 4))))))))

(define (label? obj)
  (symbol? obj))

(define (args instr)
  (if (pair? instr) (rest instr)))

(define (arg1 instr)
  (if (pair? instr) (second instr)))

(define (set-arg1! instr val)
  (set-car! (cdr instr) val))

(define (arg2 instr)
  (if (pair? instr) (third instr)))

(define (is instr op)
  (if (pair? op)
      (member? (opcode instr) op)
      (eq? (opcode instr) op)))

(define (opcode instr)
  (if (label? instr)
      'label
      (first instr)))

(define (assemble fn)
  (let* ((r1 (asm-first-pass (fn-code fn)))
	 (r2 (asm-second-pass (fn-code fn)
			      (first r1)
			      (second r1))))
    ;; stick the result on the end
    (reverse (cons r2 (reverse fn)))))

(define (asm-first-pass code)
  (let ((length 0)
	(labels nil))
    (dolist (instr code)
	    (if (label? instr)
		(push! (cons instr length) labels)
		(inc! length)))
    (list length labels)))

(define (asm-second-pass code length labels)
  (let ((addr 0)
	(code-vector (make-vector nil length)))
    (dolist (instr code)
	    (unless (label? instr)
		    (if (is instr '(jump tjump fjump save))
			(set-arg1! instr
				   (cdr (assoc (arg1 instr) labels))))
		    (set-vector! code-vector addr instr)
		    (inc! addr)))
    code-vector))

(define (comp-show fn)
  (show-fn (compiler fn) 0))


; now we can compile functions to bytecode and print the results like
; this:
; (comp-show '(if (= x y) (f (g x)) (h x y (h 1 2))))

'compiler-loaded
