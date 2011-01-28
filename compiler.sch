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

; DESCRIPTION:
;
; This is essentially a direct translation of the bytecode compiler
; presented in Peter Norvig's "Paradigms of Artificial Intelligence
; Programming." The bytecode should be identitcal to the bytecode
; produced in the book.
;
; The 'bytecodes' generated by this compiler can be executed on the
; virtual machine defined in vm.c. These bytecodes are also suitable
; for further translation and optimization. The eventual goal is
; to turn them into native assembly so user defined routines can
; execute with the same performance as interpreter primitives.


;; Comment out the second form for loads of function trace
;; information. I should really write a real trace macro at some
;; point.

(define (write-dbg1 . args)
  "display the given forms"
  (printf "debug: %a\n" args))

(define (write-dbg . args)
  "do nothing."
  #t)


(define (comp-bound? sym)
  "is the symbol defined in the compiled global environment?"
  (let* ((sentinal (gensym))
	 (result (hashtab-ref *vm-global-environment* sym sentinal)))
    (not (eq? result sentinal))))

(define (comp-global-ref sym)
  "return the global from the compiled env. error if not defined."
  (let* ((sentinal (gensym))
	 (result (cdr (hashtab-ref *vm-global-environment* sym sentinal))))
    (if (eq? result sentinal)
	(throw-error "symbol" sym "is not defined in compiled env")
	result)))

(define (comp-macro? sym)
  "is a given symbol a macro in the compiled environment?"
  (and (comp-bound? sym)
       (compiled-syntax-procedure? (comp-global-ref sym))))

(define (comp-macroexpand0 form)
  "expand form using a macro found in the compiled environment"
  (apply (comp-global-ref (car form)) (cdr form)))

(define (comp x env val? more?)
  "compile an expression in the given environment optionally caring
about its value and optionally with more forms following"
  (write-dbg 'comp x 'val? val? 'more? more?)
  (cond
   ((symbol? x) (comp-var x env val? more?))
   ((atom? x) (comp-const x val? more?))
   (else (case (first x)
	   (if-compiling (%arg-count x 2 2)
			 (comp (second x) env val? more?))
	   (quote (%arg-count x 1 1)
		  (comp-const (second x) val? more?))
	   (begin (comp-begin (rest x) env val? more?))
	   (set! (%arg-count x 2 2)
		 (seq (comp (third x) env #t #t)
		      (gen-set (second x) env)
		      (when (not val?) (gen 'pop))
		      (unless more? (gen 'return))))
	   (if (%arg-count x 2 3)
	       (comp-if (second x) (third x) (fourth x)
			env val? more?))
	   (lambda (when val?
			 (let ((f (comp-lambda (second x)
					       (rest (rest x)) env)))
			   (seq (gen 'fn f)
				(unless more? (gen 'return))))))
	   (macro (throw-error "macro is not builtin. use set-macro!"))

	   ;; generate an invocation
	   (else
	    (if (comp-macro? (first x))
		(comp (comp-macroexpand0 x) env val? more?)
		(comp-funcall (first x) (rest x)
			      env val? more?)))))))

(define (%<=2 a b)
  (or (%fixnum-less-than a b) (%fixnum-equal a b)))

(define (%<= . values)
  (every-pair? %<=2 values))

(define (%arg-count form min max)
  (let ((n-args (length (rest form))))
    (unless (%<= min n-args max)
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

(define (comp-const x val? more?)
  (write-dbg 'comp-const x 'val? val? 'more? more?)
  (when val? (seq (gen 'const x)
		  (unless more? (gen 'return)))))

(define (comp-var x env val? more?)
  (write-dbg 'comp-var x 'val? val? 'more? more?)
  (when val? (seq (gen-var x env)
		  (unless more? (gen 'return)))))

(define (false? exp)
  (or (null? exp) (eq? exp #f) (eq? exp 'nil)))

(define (true? exp)
  (eq? exp #t))

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

  (let ((prim (%primitive? f env (length args))))
    (cond
     (prim
      (if (and (not val?) (not (prim-side-effects? prim)))
	  (comp-begin args env #f more?)
	  (seq (comp-list args env)
	       (gen (prim-opcode prim))
	       (unless val? (gen 'pop))
	       (unless more? (gen 'return)))))
     ((and (starts-with? f 'lambda eq?) (null? (second f)))
      (unless (null? args) (throw-error "too many arguments"))
      (comp-begin (cdr (cdr f)) env val? more?))
     (more?
      (let ((k (gen-label 'k)))
	(seq (gen 'save k)
	     (comp-list args env)
	     (comp f env #t #t)
	     (gen 'fcallj (length args) #t)
	     (list k)
	     (unless val? (gen 'pop)))))
     (else
      (seq (comp-list args env)
	   (comp f env #t #t)
	   (gen 'fcallj (length args) #f))))))

(define (primitive-procedure? obj)
  (and (procedure? obj)
       (not (compound-procedure? obj))))

(define (environment-names . env)
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
  (if (index-eq sym (all-symbols))
      (syntax-procedure? (eval sym))
      #f))

(define-struct fn
  "a structure representing a compiled function"
  (code
   env
   name
   args))

(define (make-prim symbol n-args opcode always? side-effects?)
  (list 'prim symbol n-args opcode always? side-effects?))

(define (prim? obj)
  (starts-with? obj 'prim eq?))

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

(define *primitive-fns* '())
;  (map (lambda (fn) (cons 'prim fn))
;       '((+ 2 + #t #f) (- 2 - #t #f) (* 2 * #t #f) (/ 2 / #t #f)
;	 (< 2 < #f #f) (> 2 > #f #f)
;	 (= 2 = #f #f) (eq? 2 eq? #f #f)
;	 (car 1 car #f #f) (cdr 1 cdr #f #f) (cons 2 cons #f #f)
;	 (car0 1 car #f #f) (cdr0 1 cdr #f #f)
;	 (null? 1 null? #f #f))))

;; f is primitive if it's in the table and not shadowed in the
;; environment and has the right number of arguments.
(define (%primitive? f env n-args)
  (write-dbg 'primitive? f 'env env 'n-args n-args)
  (and (not (in-env? f env))
       (find (lambda (p)
	       (and (eq? f (prim-symbol p))
		    (%fixnum-add n-args (prim-n-args p))))
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
  (new-fun (seq (%gen-args args 0)
		(comp-begin body
			    (cons (make-true-list args) env)
			    #t #f))
	   env "unknown" args))

(define (%gen-args args n-so-far)
  (cond
   ((null? args) (gen 'args n-so-far))
   ((symbol? args) (gen 'argsdot n-so-far))
   ((and (pair? args)
	 (symbol? (first args)))
    (%gen-args (rest args) (%fixnum-add n-so-far 1)))
   (else (throw-error "illegal argument list" args))))

;; this doesn't do error checking like the method before
(define (num-args args)
  (letrec ((iter (lambda (lst count)
		   (cond
		    ((null? lst) count)
		    ((symbol? lst) (%fixnum-add count 1))
		    (else (iter (rest lst) (%fixnum-add count 1)))))))
    (iter args 0)))

(define (make-true-list dotted-list)
  (cond
   ((null? dotted-list) nil)
   ((atom? dotted-list) (list dotted-list))
   (else (cons (first dotted-list)
	       (make-true-list (rest dotted-list))))))

(define (new-fun code env name args)
  (assemble (make-fn 'code (optimize code)
		     'env env
		     'name name
		     'args args)))

(let ((label-num 0))
  (define (compiler x)
    (set! label-num 0)
    (comp-lambda nil (list x) nil))

  (define (gen-label . opt)
    (let ((prefix (if (pair? opt)
		      (string (car opt))
		      "L")))
      (write-dbg 'gen-label prefix)
      (set! label-num (%fixnum-add label-num 1))
      (string->symbol
       (prim-concat prefix (number->string label-num))))))

(define (gen opcode . args)
  (write-dbg 'gen opcode 'args args)
  (list (cons opcode args)))

(define (seq . code)
  (append-all code))

(define (string obj)
  (cond
   ((string? obj) obj)
   ((symbol? obj) (symbol->string obj))
   (else (throw-error "can't make" obj "a string"))))

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
	    (throw-error "can't alter the constant" var)
	    (gen 'gset var)))))

(define (in-env? symbol env)
  (let ((frame (find (lambda (f) (member? symbol f)) env)))
    (if (not frame)
	nil
	(list (index-eq frame env) (index-eq symbol frame)))))

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

(define (map-vector fn vector)
  (let* ((len (vector-length vector))
	 (result (make-vector len nil)))
    (dotimes (idx len)
      (vector-set! result idx
		   (fn (vector-ref vector idx))))
    result))

(define (instrs-to-bytes instr-vector)
  (let* ((len (vector-length instr-vector))
	 (result (ffi:make-longs (%fixnum-mul 3 len))))

    (dotimes (idx len)
      (let ((instr (vector-ref instr-vector idx))
	    (off (%fixnum-mul idx 3)))

	(ffi:long-set! result off (char->integer (opcode instr)))

	(if (cdr instr)
	    (begin
	      (ffi:long-set! result (%fixnum-add off 1)
			     (cadr instr))
	      (if (cddr instr)
		  (begin
		    (ffi:long-set! result (%fixnum-add off 2)
				   (caddr instr)))
		  ;; no second arg
		  (ffi:long-set! result (%fixnum-add off 2) -1)))

	    (begin
	      ;; no first or second arg
	      (ffi:long-set! result (%fixnum-add off 1) -1)

	      (ffi:long-set! result (%fixnum-add off 2) -1)))))


    result))

(define (build-const-table instrs)
  (let ((result nil)
	(idx 0))

    (dolist (inst instrs)
      (when (is inst '(const fn gvar gset))
        (push! (arg1 inst) result)
	(set-car! (cdr inst) idx)
	(%inc! idx)))

    (apply vector (reverse result))))

(define (assemble fn)
  (let* (;; determine the value of each symbolic label and remove
	 ;; those labels from the instruction stream
	 (r1 (asm-first-pass (fn-code-ref fn)))

	 ;; while everything is still symbolic we extract the consts
	 ;; and mutate the arg of the old instruction to point into
	 ;; the table
	 (consts (build-const-table (fn-code-ref fn)))

	 ;; resolve all jumps and convert the instrs into characters
	 (instrs (asm-second-pass (fn-code-ref fn)
				  (first r1)
				  (second r1)))

	 ;; remember the number of instructions in the stream since
	 ;; the alien byte array doesn't store its length
	 (num-bytes (%fixnum-mul (vector-length instrs) 3))

	 ;; pack the instructions into the final alien byte array
	 (bytes (instrs-to-bytes instrs)))

    ;; pack the final compiled proc
    (make-compiled-proc (list num-bytes bytes consts)
			      (fn-env-ref fn))))


(define (asm-first-pass code)
  (let ((length 0)
	(labels nil))
    (dolist (instr code)
	    (if (label? instr)
		(push! (cons instr length) labels)
		(%inc! length)))
    (list length labels)))

(define (asm-second-pass code length labels)
  (let ((addr 0)
	(code-vector (make-vector length nil)))
    (dolist (instr code)
	    (unless (label? instr)
		    (if (is instr '(jump tjump fjump save))
			(set-arg1! instr
				   (cdr (assoc (arg1 instr) labels))))

		    ;; if this has a bytecode, convert it
		    (let ((bytecode (symbol->bytecode (opcode instr))))
		      (if bytecode
			  (set-car! instr bytecode)))

		    (vector-set! code-vector addr instr)
		    (%inc! addr)))
    code-vector))

(define (fn-opcode? instr)
  (is instr 'fn))

(define (optimize code)
  (when (not (any? fn-opcode? code))
    ;; nothing closed over our environment so we can recycle it
    (dolist (op code)
      (when (and (is op 'fcallj)
		 (not (arg2 op)))
	    (set-car! op 'callj))))
  code)


(define (make-space spaces)
  (make-string spaces #\space))

(define (%show-fn fn indent)
  (map display (list "consts: " (caddr (compiled-bytecode fn))))
  (newline)

  (let ((line-num 0)
	(len (/ (car (compiled-bytecode fn)) 3))
	(bytes (cadr (compiled-bytecode fn)))
	(consts (caddr (compiled-bytecode fn))))

    (dotimes (idx len)
      (let* ((off (* idx 3))
	     (instr (ffi:long-ref bytes off))
	     (arg1 (ffi:long-ref bytes (+ off 1)))
	     (arg2 (ffi:long-ref bytes (+ off 2))))

	(let* ((opcode-sym (bytecode->symbol (integer->char instr)))
	       (sym (if opcode-sym opcode-sym instr))
	       (instr (list sym arg1 arg2)))

	  (if (is instr 'fn)
	      (begin
		(map display (list line-num ": " (make-space indent) "fn "))
		(%show-fn (vector-ref consts (second instr))
			  (%fixnum-add indent 4)))
	      (begin
		(map display (list line-num ": " (make-space indent) instr))
		(newline))))
	(inc! line-num)))))


(define (comp-show fn)
  (%show-fn (compiler fn) 0))

(define (dump-compiled-fn fn . indent)
  (let ((indent (if (null? indent)
		    0
		    (car indent))))
    (%show-fn fn indent)))

(define (comp-repl)
  (display "comp-repl> ")
  (let ((result ((compiler (read-port stdin)))))
    (write-port result stdout)
    (newline)
    (unless (eq? result 'quit)
	    (comp-repl))))

; now we can compile functions to bytecode and print the results like
; this:
; (comp-show '(if (= x y) (f (g x)) (h x y (h 1 2))))


(define (compiling-load-eval form env)
  (let ((result ((compiler form))))
    result))

(define (compile-file name)
  "read and compile all forms in file"
  (let ((file (find-library name)))
    (if file
        (letrec ((in (open-input-port file))
                 (iter (lambda (form)
                         (unless (eof-object? form)
                           ((compiler form))
                           (iter (read-port in))))))
          (if (eof-object? in)
              (throw-error "compiler failed to open" file)
              (iter (read-port in)))
          #t)
        (throw-error "failed to find" name))))

(provide 'compiler)

