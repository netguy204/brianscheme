; Mode: Scheme
;
;
; **********************************************************************
; Copyright (c) 1992 Xerox Corporation.
; All Rights Reserved.
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; **********************************************************************
;
; EDIT HISTORY:
;
;      10/**/92  Gregor  Originally Written
; 1.0  11/10/92  Gregor  Changed names of generic invocation generics.
;                        Changed compute-getters-and-setters protocol.
;                        Made comments match the code.
;                        Changed maximum line width to 72.
; 1.1  11/24/92  Gregor  Fixed bug in compute-method-more-specific?,
;                        wrt the use of for-each.
;                        Both methods on allocate instance failed to
;                        initialize fields properly.
;                        The specializers and procedure initargs are
;                        now required when creating a method, that is,
;                        they no longer default.  No working program
;                        should notice this change.
; 1.2  12/02/92  Gregor  Fix minor things that improve portability:
;                         - DEFINE needs 2 args in R4Rs
;                         - Conditionalize printer hooks.
;                         - () doesn't evaluate to ()
;
; 1.3  12/08/92  Gregor  More minor things:
;                         - () really doesn't evaluate to () damnit!
;                         - It turns out DEFINE-MACRO is never used.
;                         - Confusion over the "failure" return value
;                           of ASSQ -- ASSQ returns #f if the key is
;                           not found.
;                         - SEQUENCE   --> BEGIN
;                         - LAST-PAIR  --> last now in support
;                        Change instance rep to protect Schemes that
;                        don't detect circular structures when
;                        printing.
;                        A more reasonable error message when there
;                        are no applicable methods or next methods.
; 1.4  12/10/92  Gregor  Flush filter-in for collect-if.  Add news
;                        classes <input-port> and <output-port>.
;                        Also add
;
; 1.5  12/17/92  Gregor  Minor changes to class of and primitive
;                        classes to try and deal with '() and #f
;                        better.
;
; 1.6   9/9/93   Gregor  Fix a monstrous bug in the bootstrap of
;                        compute-apply-generic which sometimes ran
;                        user methods on this generic function when
;                        it shouldn't.
;
; 1.7   8/9/94   Gregor  Add Scheme 48 to support.scm.
;
;
;
(define tiny-clos-version "1.7")

'(;Stuff to make emacs more reasonable.

  (put 'letrec 'lisp-indent-hook 1)

  (put 'make-method  'lisp-indent-hook 1)
  (put 'add-method   'lisp-indent-hook 'defun)

 )
;
; A very simple CLOS-like language, embedded in Scheme, with a simple
; MOP.  The features of the default base language are:
;
;   * Classes, with instance slots, but no slot options.
;   * Multiple-inheritance.
;   * Generic functions with multi-methods and class specializers only.
;   * Primary methods and call-next-method; no other method combination.
;   * Uses Scheme's lexical scoping facilities as the class and generic
;     function naming mechanism.  Another way of saying this is that
;     class, generic function and methods are first-class (meta)objects.
;
; While the MOP is simple, it is essentially equal in power to both MOPs
; in AMOP.  This implementation is not at all optimized, but the MOP is
; designed so that it can be optimized.  In fact, this MOP allows better
; optimization of slot access extenstions than those in AMOP.
;
;
;
; In addition to calling a generic, the entry points to the default base
; language are:
;
;   (MAKE-CLASS list-of-superclasses list-of-slot-names)
;   (MAKE-GENERIC)
;   (MAKE-METHOD list-of-specializers procedure)
;   (ADD-METHOD generic method)
;
;   (MAKE class . initargs)
;   (INITIALIZE instance initargs)            ;Add methods to this,
;                                             ;don't call it directly.
;
;   (SLOT-REF  object slot-name)
;   (SLOT-SET! object slot-name new-value)
;
;
; So, for example, one might do:
;
;   (define <position> (make-class (list <object>) (list 'x 'y)))
;   (add-method initialize
;       (make-method (list <position>)
;         (lambda (call-next-method pos initargs)
;           (for-each (lambda (initarg-name slot-name)
;                       (slot-set! pos
;                                  slot-name
;                                  (getl initargs initarg-name 0)))
;                     '(x y)
;                     '(x y)))))
;
;   (set! p1 (make <position> 'x 1 'y 3))
;
;
;
; NOTE!  Do not use EQUAL? to compare objects!  Use EQ? or some hand
;        written procedure.  Objects have a pointer to their class,
;        and classes are circular structures, and ...
;
;
;
; The introspective part of the MOP looks like the following.  Note that
; these are ordinary procedures, not generics.
;
;   CLASS-OF
;
;   CLASS-DIRECT-SUPERS
;   CLASS-DIRECT-SLOTS
;   CLASS-CPL
;   CLASS-SLOTS
;
;   GENERIC-METHODS
;
;   METHOD-SPECIALIZERS
;   METHOD-PROCEDURE
;
;
; The intercessory protocol looks like (generics in uppercase):
;
;   make
;     ALLOCATE-INSTANCE
;     INITIALIZE                   (really a base-level generic)
;
;   class initialization
;     COMPUTE-CPL
;     COMPUTE-SLOTS
;     COMPUTE-GETTER-AND-SETTER
;
;   add-method                     (Notice this is not a generic!)
;     COMPUTE-APPLY-GENERIC
;       COMPUTE-METHODS
;         COMPUTE-METHOD-MORE-SPECIFIC?
;       COMPUTE-APPLY-METHODS
;

;
; OK, now let's get going.  But, as usual, before we can do anything
; interesting, we have to muck around for a bit first.  First, we need
; to load the support library.
;

(require "clos/support.sch")

;
; Then, we need to build what, in a more real implementation, would be
; the interface to the memory subsystem: instances and entities.  The
; former are used for instances of instances of <class>; the latter
; are used for instances of instances of <entity-class>.  In this MOP,
; none of this is visible to base- or MOP-level programmers.
;
; A few things to note, that have influenced the way all this is done:
;
;   - R4RS doesn't provide a mechanism for specializing the
;     behavior of the printer for certain objects.
;
;   - Some Scheme implementations bomb when printing circular
;     structures -- that is, arrays and/or lists that somehow
;     point back to themselves.
;
; So, the natural implementation of instances -- vectors whose first
; field point to the class -- is straight on out.  Instead, we use a
; procedure to `encapsulate' that natural representation.
;
; Having gone that far, it makes things simpler to unify the way normal
; instances and entities are handled, at least in the lower levels of
; the system.  Don't get faked out by this -- the user shouldn't think
; of normal instances as being procedures, they aren't. (At least not
; in this language.)  If you are using this to teach, you probably want
; to hide the implementation of instances and entities from people.
;
;
(define (%allocate-instance class nfields)
  (%allocate-instance-internal
   class #t
   (lambda args
     (throw-error "An instance isn't a procedure -- can't apply it."))
   nfields))

(define (%allocate-entity class nfields)
  (%allocate-instance-internal
   class #f
   (lambda args
     (throw-error "Tried to call an entity before its proc is set."))
   nfields))

;; Instances and entities are actually closures over a vector. the
;; closure is meta-wrap'd so that it's distinguishable from other
;; closures and the closure will return its internal vector to the
;; caller if the uninterned metatag symbol is passed to it. Nothing
;; outside of this let* should care that there happens to be a vector
;; inside the closure... it's just implementation detail.
(let* ((metatag (gensym))
       (get-vector
	(lambda (closure)
	  (if (and (meta? closure)
		   (pair? (meta-data closure))
		   (eq? (car (meta-data closure)) metatag))
	      (cdr (meta-data closure))
	      nil))))

  (define (%allocate-instance-internal class lock proc nfields)
    (letrec ((vector (make-vector (+ nfields 3) #f))
	     (closure (lambda args
			(apply (vector-ref vector 0) args))))

      (vector-set! vector 0 proc)
      (vector-set! vector 1 lock)
      (vector-set! vector 2 class)
      (meta-wrap closure (cons metatag vector))))

  (define (%instance? x)
    (not (null? (get-vector x))))

  (define (%instance-class closure)
    (let ((vector (get-vector closure)))
      (vector-ref vector 2)))

  (define (%set-instance-class-to-self closure)
    (let ((vector (get-vector closure)))
      (vector-set! vector 2 closure)))

  (define (%set-instance-proc! closure proc)
    (let ((vector (get-vector closure)))
      (if (vector-ref vector 1)
	  (throw-error "Can't set procedure of instance.")
	  (vector-set! vector 0 proc))))

  (define (%instance-ref closure index)
    (let ((vector (get-vector closure)))
      (vector-ref vector (+ index 3))))

  (define (%instance-set! closure index new-value)
    (let ((vector (get-vector closure)))
      (vector-set! vector (+ index 3) new-value))))


;
; %allocate-instance, %allocate-entity, %instance-ref, %instance-set!
; and class-of are the normal interface, from the rest of the code, to
; the low-level memory system.  One thing to take note of is that the
; protocol does not allow the user to add low-level instance
; representations.  I have never seen a way to make that work.
;
; Note that this implementation of class-of assumes the name of a the
; primitive classes that are set up later.
;
(define (class-of x)
  (cond
   ((%instance? x)   (%instance-class x))
   ((pair? x)        <pair>)
   ((lazy-symbol? x) <lazy-symbol>)
   ((symbol? x)      <symbol>)
   ((string? x)      <string>)
   ((integer? x)     <integer>)
   ((real? x)        <real>)
   ((boolean? x)     <boolean>)
   ((char? x)        <char>)
   ((null? x)        <null>)
   ((compiled-syntax-procedure? x) <compiled-syntax-procedure>)
   ((compiled-procedure? x) <compiled-procedure>)
   ((vector? x)      <vector>)
   ((hashtab? x)     <hashtab>)
   ((alien? x)       <alien>)
   ((number? x)      <number>)
   ((input-port? x)  <input-port>)
   ((output-port? x) <output-port>)
   ((syntax-procedure? x) <syntax-procedure>)
   ((procedure? x)   <procedure>)
   ((directory-stream? x) <directory-stream>)
   ((small-integer? x) <small-integer>)))


;
; Now we can get down to business.  First, we initialize the braid.
;
; For Bootstrapping, we define an early version of MAKE.  It will be
; changed to the real version later on.  String search for ``set! make''.
;

(define (make class . initargs)
  (cond
   ((or (eq? class <class>)
	(eq? class <entity-class>))

    (let* ((new (%allocate-instance
		 class
		 (length the-slots-of-a-class)))
	   (dsupers (getl initargs 'direct-supers '()))
	   (class-name (getl initargs 'class-name 'unknown))
	   (dslots  (map list
			 (getl initargs 'direct-slots  '())))
	   (cpl     (let loop ((sups dsupers)
			       (so-far (list new)))
		      (if (null? sups)
			  (reverse so-far)
			  (loop (class-direct-supers
				 (car sups))
				(cons (car sups)
				      so-far)))))
	   (slots (apply append
			 (cons dslots
			       (map class-direct-slots
				    (cdr cpl)))))
	   (nfields 0)
	   (field-initializers '())
	   (allocator
	    (lambda (init)
	      (let ((f nfields))
		(set! nfields (+ nfields 1))
		(push! init field-initializers)
		(list (lambda (o)   (%instance-ref  o f))
		      (lambda (o n) (%instance-set! o f n))))))
	   (getters-n-setters
	    (let ((ht (make-hashtab-eq 10)))
	      (dolist (s slots)
		      (hashtab-set! ht (car s)
				    (allocator (lambda () '()))))
	      ht)))

      (slot-set! new 'direct-supers      dsupers)
      (slot-set! new 'direct-slots       dslots)
      (slot-set! new 'class-name         class-name)
      (slot-set! new 'cpl                cpl)
      (slot-set! new 'slots              slots)
      (slot-set! new 'nfields            nfields)
      (slot-set! new 'field-initializers (reverse
					  field-initializers))

      (slot-set! new 'getters-n-setters  getters-n-setters)
      new))

   ((eq? class <generic>)
    (let ((new (%allocate-entity class
				 (length (class-slots class)))))
      (slot-set! new 'methods '())
      (slot-set! new 'invalidators '())
      new))

   ((eq? class <method>)
    (let ((new (%allocate-instance
		class
		(length (class-slots class)))))
      (slot-set! new
		 'specializers
		 (getl initargs 'specializers))
      (slot-set! new
		 'procedure
		 (getl initargs 'procedure))
      new))))

;
; These are the real versions of slot-ref and slot-set!.  Because of the
; way the new slot access protocol works, with no generic call in line,
; they can be defined up front like this.  Cool eh?
;
;
(define (slot-ref object slot-name)
  (let* ((info (lookup-slot-info (class-of object) slot-name))
	 (getter (list-ref info 0)))
    (getter object)))

(define (slot-set! object slot-name new-value)
  (let* ((info (lookup-slot-info (class-of object) slot-name))
	 (setter (list-ref info 1)))
    (setter object new-value)))

(let ((gns-cache (make-hashtab-eq 30)))
  (define (lookup-slot-info class slot-name)
    (let ((cached (hashtab-ref gns-cache class #f)))
      (let* ((getters-n-setters
	      (cond
	       (cached cached)
	       ((eq? class <class>)           ;* This grounds out
		getters-n-setters-for-class)  ;* the slot-ref tower.
	       (else (slot-ref class 'getters-n-setters))))
	     (entry (hashtab-ref getters-n-setters slot-name nil)))

	(unless cached
	  (hashtab-set! gns-cache class getters-n-setters))

	(if entry
	    entry
	    (throw-error "No slot" slot-name "in instances of" class))))))

;
; Given that the early version of MAKE is allowed to call accessors on
; class metaobjects, the definitions for them come here, before the
; actual class definitions, which are coming up right afterwards.
;
;
(define (class-direct-slots class)
  (slot-ref class 'direct-slots))

(define (class-direct-supers class)
  (slot-ref class 'direct-supers))

(define (class-slots class)
  (slot-ref class 'slots))

(define (class-cpl class)
  (slot-ref class 'cpl))

(define (generic-methods generic)
  (slot-ref generic 'methods))

(define (method-specializers method)
  (slot-ref method 'specializers))

(define (method-procedure method)
  (slot-ref method 'procedure))

(define (instance-of? class instance)
  "true if instance is of type class or one of its subtypes"
  (and (memq class (class-cpl (class-of instance))) #t))

;
; The next 7 clusters define the 6 initial classes.  It takes 7 to 6
; because the first and fourth both contribute to <class>.
;
(define the-slots-of-a-class     ;
  '(direct-supers              ;(class ...)
    direct-slots               ;((name . options) ...)
    class-name
    cpl                        ;(class ...)
    slots                      ;((name . options) ...)
    nfields                    ;an integer
    field-initializers         ;(proc ...)
    getters-n-setters))        ;((slot-name getter setter) ...)

;(define getters-n-setters-for-class      ;see lookup-slot-info
;  (let ((result nil))
;    (dolist-idx ((slot idx) the-slots-of-a-class)
;      (push! (list slot
;		   (lambda (o) (%instance-ref o idx))
;		   (lambda (o n) (%instance-set! o idx n)))
;	     result))
;    (reverse result)))

(define getters-n-setters-for-class
    ;
    ; I know this seems like a silly way to write this.  The
    ; problem is that the obvious way to write it seems to
    ; tickle a bug in MIT Scheme!
    ;
  (let ((ht (make-hashtab-eq 10))
	(make-em (lambda (s f)
		   (list
		    (lambda (o)   (%instance-ref  o f))
		    (lambda (o n) (%instance-set! o f n))))))

    (dolist (s the-slots-of-a-class)
	    (hashtab-set! ht s
			  (make-em s (position-of s the-slots-of-a-class))))
    ht))

(define <class>
  (begin
    (%allocate-instance #f (length the-slots-of-a-class))))

(%set-instance-class-to-self <class>)

(define <top>
  (make <class>
    'direct-supers (list)
    'direct-slots  (list)
    'class-name '<top>))

(define <object>
  (make <class>
    'direct-supers (list <top>)
    'direct-slots  (list)
    'class-name '<object>))

;
; This cluster, together with the first cluster above that defines
; <class> and sets its class, have the effect of:
;
;   (define <class>
;     (make <class>
;           'direct-supers (list <object>)
;           'direct-slots  (list 'direct-supers ...)))
;

(slot-set! <class> 'direct-supers
	   (list <object>))

(slot-set! <class> 'direct-slots
	   (map list the-slots-of-a-class))

(slot-set! <class> 'class-name
	   '<class>)

(slot-set! <class> 'cpl
	   (list <class> <object> <top>))

(slot-set! <class> 'slots
	   (map list the-slots-of-a-class))

(slot-set! <class> 'nfields
	   (length the-slots-of-a-class))

(slot-set! <class> 'field-initializers
	   (map (lambda (s)
		  (lambda () '()))
		the-slots-of-a-class))

;; when we (make) instances of the metaclass <class> we'll build the
;; appropriate getters-n-setters. But we don't need to be
;; getting-n-setting directly on the metaclass so those get turned
;; off.
(slot-set! <class> 'getters-n-setters (make-hashtab-eq 10))


;; now are basic make is useful since <class> is fully defined though
;; we can really only use it to make things based on the metaclasses
;; that simple-make is hardcoded to know about
(define <procedure-class>
  (make <class>
    'direct-supers (list <class>)
    'class-name '<procedure-class>))

(define <entity-class>
  (make <class>
    'direct-supers (list <procedure-class>)
    'class-name '<entity-class>))

(define <generic>
  (make <entity-class>
    'direct-supers (list <object>)
    'direct-slots (list 'methods 'invalidators)
    'class-name '<generic>))

(define <method>
  (make <class>
    'direct-supers (list <object>)
    'direct-slots (list 'specializers
			'procedure)
    'class-name '<method>))

;
; These are the convenient syntax we expose to the base-level user.
;
;
(define (make-class direct-supers direct-slots)
  (make <class>
    'direct-supers direct-supers
    'direct-slots  direct-slots))

(define (make-generic)
  (make <generic>))

(define (generic-function? closure)
  (and (%instance? closure)
       (instance-of? <generic> closure)))

(define (make-method specializers procedure)
  (make <method>
    'specializers specializers
    'procedure    procedure))

(define-syntax (define-generic name . documentation)
  "syntax for declaring new generic functions"
  (when documentation
	(add-documentation name (car documentation)))
  `(define ,name (make-generic)))


;
; The initialization protocol
;
(define-generic initialize
  "initialize a new instance with the arguments given to make")

;
; The instance structure protocol.
;
(define-generic allocate-instance
  "allocate a new instance")

(define-generic compute-getter-and-setter
  "computer the getters and setters for an instance")

;
; The class initialization protocol.
;
(define-generic compute-cpl
  "compute the class priority list")

(define-generic compute-slots
  "compute the slots")

;
; The generic invocation protocol.
;
(define-generic compute-apply-generic
  "build the method that's called when a generic is applied")

(define-generic compute-methods)

(define-generic compute-method-more-specific?)

(define-generic compute-apply-methods)

;
; The next thing to do is bootstrap generic functions.
;
(define generic-invocation-generics
  (list compute-apply-generic
	compute-methods
	compute-method-more-specific?
	compute-apply-methods))

(define (add-method generic method)
  (slot-set! generic
	     'methods
	     (cons method
		   (collect-if
		    (lambda (m)
		      (not (every eq?
				(method-specializers m)
				(method-specializers method))))
		    (slot-ref generic 'methods))))
  (%set-instance-proc! generic (compute-apply-generic generic)))


(define-syntax (define-method name-and-specialized-args . body)
  "syntax for adding a method to a generic function"
  (let ((args (gensym))
	(method (gensym)))
    `(let ((,method
	    (make-method (list . ,(map second
				       (filter pair?
					       (rest name-and-specialized-args))))
			 (lambda (call-next-method
				  . ,(map (lambda (v)
					    (if (pair? v)
						(first v)
						v))
					  (rest name-and-specialized-args)))
			   . ,body))))
       (add-method ,(first name-and-specialized-args) ,method)
       ,method)))


; Adding a method calls COMPUTE-APPLY-GENERIC, the result of which calls
; the other generics in the generic invocation protocol.  Two, related,
; problems come up.  A chicken and egg problem and a infinite regress
; problem.
;
; In order to add our first method to COMPUTE-APPLY-GENERIC, we need
; something sitting there, so it can be called.  The first definition
; below does that.
;
; Then, the second definition solves both the infinite regress and the
; not having enough of the protocol around to build itself problem the
; same way: it special cases invocation of generics in the invocation
; protocol.
;

(%set-instance-proc! compute-apply-generic
  (lambda (generic)
    (let ((method (car (generic-methods generic))))
      ((method-procedure method) #f generic))))

(define-method (compute-apply-generic (generic <generic>))
  (lambda args
    (if (and (memq generic generic-invocation-generics)     ;* G  c
	     (memq (car args) generic-invocation-generics)) ;* r  a
	(apply (method-procedure                            ;* o  s
		(last (generic-methods generic)))           ;* u  e
	       (cons #f args))                              ;* n
	                                                    ;* d
	((compute-apply-methods generic)
	 ((compute-methods generic) args)
	 args))))


(define-method (compute-methods (generic <generic>))
  (lambda (args)
    (let ((applicable
	   (collect-if (lambda (method)
			 ;;
			 ;; Note that every only goes as far as the
			 ;; shortest list!
			 ;;
			 (every applicable?
				(method-specializers method)
				args))
		       (generic-methods generic))))
      (let ((method-more-specific?
	     (compute-method-more-specific? generic)))

	(gsort (lambda (m1 m2)
		 (method-more-specific? m1 m2 args)) applicable)))))


(define-method (compute-method-more-specific? (generic <generic>))
  (lambda (m1 m2 args)
    (let loop ((specls1 (method-specializers m1))
	       (specls2 (method-specializers m2))
	       (args args))
      (cond
       ((and (null? specls1) (null? specls2))
	(throw-error
	 "Two methods are equally specific."))
       ((or  (null? specls1) (null? specls2))
	(throw-error
	 "Two methods have a different number of specializers."))
       ((null? args)
	(throw-error
	 "Fewer arguments than specializers."))
       (else
	(let ((c1  (car specls1))
	      (c2  (car specls2))
	      (arg (car args)))
	  (if (eq? c1 c2)
	      (loop (cdr specls1)
		    (cdr specls2)
		    (cdr args))
	      (more-specific? c1 c2 arg))))))))


(define-method (compute-apply-methods (generic <generic>))
  (lambda (methods args)
    (letrec ((one-step
	      (lambda (tail)
		(lambda ()
		  (if (null? tail)
		      (throw-error "No applicable methods/next methods.")
		      (apply (method-procedure (car tail))
			     (cons (one-step (cdr tail)) args)))))))
      ((one-step methods)))))

(define (applicable? c arg)
  (let ((cls (class-of arg)))
    (cond
     ((eq? cls c) #t)
     (else (memq c (class-cpl cls))))))

(define (more-specific? c1 c2 arg)
  (let ((cls (class-of arg)))
    (cond
     ((eq? cls c1) #t)
     ((eq? cls c2) #f)
     (else
      (memq c2 (memq c1 (class-cpl cls)))))))


;
; now we define some of the generic function/method stack again in a
; way that caches as much information as possible. We couldn't so this
; before because the pre-computations would have depended on generic
; methods that weren't implemented yet.
;

(define-method (compute-apply-generic (generic <generic>))
  (let ((method-applier (compute-apply-methods generic))
	(method-computer (compute-methods generic)))

    (lambda args
      (method-applier (method-computer args) args))))


(define-method (initialize (object <object>) initargs)
  object)

(define-method (initialize (class <class>) initargs)
  (call-next-method)
  (slot-set! class
	     'direct-supers
	     (getl initargs 'direct-supers '()))
  (slot-set! class
	     'direct-slots
	     (map (lambda (s)
		    (if (pair? s)
			s
			(list s)))
		  (getl initargs 'direct-slots  '())))
  (slot-set! class
	     'class-name
	     (getl initargs 'class-name 'unknown))
  (slot-set! class 'cpl   (compute-cpl   class))
  (slot-set! class 'slots (compute-slots class))
  (let* ((nfields 0)
	 (field-initializers '())
	 (allocator
	  (lambda (init)
	    (let ((f nfields))
	      (set! nfields (+ nfields 1))
	      (set! field-initializers
		    (cons init field-initializers))
	      (list (lambda (o)   (%instance-ref  o f))
		    (lambda (o n) (%instance-set! o f n))))))
	 (getters-n-setters
	  (let ((ht (make-hashtab-eq 10)))
	    (dolist (slot (slot-ref class 'slots))
		    (hashtab-set! ht (car slot)
				  (compute-getter-and-setter class
							     slot
							     allocator)))
	    ht)))

    (slot-set! class 'nfields nfields)
    (slot-set! class 'field-initializers field-initializers)
    (slot-set! class 'getters-n-setters getters-n-setters)))

(define-method (initialize (generic <generic>) initargs)
  (call-next-method)
  (slot-set! generic 'methods '())
  (%set-instance-proc! generic
		       (lambda args (throw-error "Has no methods."))))

(define-method (initialize (method <method>) initargs)
  (call-next-method)
  (slot-set! method 'specializers (getl initargs 'specializers))
  (slot-set! method 'procedure    (getl initargs 'procedure)))

(define-method (allocate-instance (class <class>))
  (let* ((field-initializers (slot-ref class 'field-initializers))
	 (new (%allocate-instance
	       class
	       (length field-initializers))))
    (let loop ((n 0)
	       (inits field-initializers))
      (if (pair? inits)
	  (begin
	    (%instance-set! new n ((car inits)))
	    (loop (+ n 1)
		  (cdr inits)))
	  new))))

(define-method (allocate-instance (class <entity-class>))
  (let* ((field-initializers (slot-ref class 'field-initializers))
	 (new (%allocate-entity
	       class
	       (length field-initializers))))
    (let loop ((n 0)
	       (inits field-initializers))
      (if (pair? inits)
	  (begin
	    (%instance-set! new n ((car inits)))
	    (loop (+ n 1)
		  (cdr inits)))
	  new))))

(define-method (compute-cpl (class <class>))
  (compute-std-cpl class class-direct-supers))

(define-method (compute-slots (class <class>))
  (let collect ((to-process (apply append
				   (map class-direct-slots
					(class-cpl class))))
		(result '()))
    (if (null? to-process)
	(reverse result)
	(let* ((current (car to-process))
	       (name (car current))
	       (others '())
	       (remaining-to-process
		(collect-if (lambda (o)
			      (if (eq? (car o) name)
				  (begin
				    (set! others (cons o others))
				    #f)
				  #t))
			    (cdr to-process))))
	  (collect remaining-to-process
		   (cons (append current
				 (apply append (map cdr others)))
			 result))))))

(define-method (compute-getter-and-setter (class <class>) slot allocator)
  (allocator (lambda () '())))


;
; Now everything works, both generic functions and classes, so we can
; turn on the real MAKE.
;
;
(define (make class . initargs)
  (let ((instance (allocate-instance class)))
    (initialize instance initargs)
    instance))


;
; Now define what CLOS calls `built in' classes.
;
;
(define <primitive-class>
  (make <class>
    'direct-supers (list <class>)
    'class-name    '<primitive-class>))

(define (make-primitive-class class name)
  (make (if (null? class)
	    <primitive-class>
	    class)
    'direct-supers (list <top>)
    'class-name name))


(define <pair>        (make-primitive-class nil '<pair>))
(define <null>        (make-primitive-class nil '<null>))
(define <symbol>      (make-primitive-class nil '<symbol>))
(define <boolean>     (make-primitive-class nil '<boolean>))
(define <procedure>   (make-primitive-class <procedure-class> '<procedure>))
(define <syntax-procedure>
  (make-primitive-class <procedure-class> '<syntax-procedure>))

(define <compiled-procedure>
  (make-primitive-class <procedure-class> '<compiled-procedure>))
(define <compiled-syntax-procedure>
  (make-primitive-class <procedure-class> '<compiled-syntax-procedure>))

(define <number>      (make-primitive-class nil '<number>))
(define <integer>     (make <class>
			'direct-supers (list <number>)
			'class-name '<integer>))
(define <small-integer>     (make <class>
			      'direct-supers (list <number>)
			      'class-name '<small-integer>))
(define <real>        (make <class>
			'direct-supers (list <number>)
			'class-name '<real>))
(define <vector>      (make-primitive-class nil '<vector>))
(define <hashtab>     (make-primitive-class nil '<hashtab>))
(define <char>        (make-primitive-class nil '<char>))
(define <string>      (make-primitive-class nil '<string>))
(define <alien>       (make-primitive-class nil '<alien>))
(define <input-port>  (make-primitive-class nil '<input-port>))
(define <output-port> (make-primitive-class nil '<output-port>))
(define <directory-stream> (make-primitive-class nil '<directory-stream>))
(define <lazy-symbol> (make-primitive-class nil '<lazy-symbol>))


; now we can override this since all of our primitive classes
; are finally available
(define-method (compute-methods (generic <generic>))
  (let ((more-specific?
	 (compute-method-more-specific? generic))
	(methods (generic-methods generic))
	(method-cache nil))

    (lambda (args)
      (let* ((arg-classes (map class-of args))
	     (match (assoc arg-classes method-cache)))
	(if match
	    ;; return from cache
	    (rest match)
	    ;; missed cache. compute a new value
	    (let ((applicable
		   (collect-if (lambda (method)
				 (every applicable?
					(method-specializers method)
					args))
			       methods)))

	      (let ((result (gsort (lambda (m1 m2)
				     (more-specific? m1 m2 args))
				   applicable)))
		(push! (cons arg-classes result)
		       method-cache)
		result)))))))


; make sure everyone is using the new cacheing version
(dolist (generic (list initialize allocate-instance
		       compute-getter-and-setter compute-cpl
		       compute-slots compute-apply-generic
		       compute-methods compute-method-more-specific?
		       compute-apply-methods))

	(%set-instance-proc! generic (compute-apply-generic generic)))


(define-generic invalidate-caches
  "invalidate any caches attached to a generic")

(define-method (invalidate-caches (generic <generic>))
  (let ((invalidators (slot-ref generic 'invalidators)))
    (dolist (fn invalidators)
      (fn))
    (slot-set! generic 'invalidators nil)))

(define-generic add-invalidator
  "attach an invalidation hook to a generic")

(define-method (add-invalidator (generic <generic>) fn)
  (slot-set! generic 'invalidators
	     (cons fn (slot-ref generic 'invalidators))))

;; install the cache invalidating form of add-method
(let ((old-add-method add-method))
  (define (add-method generic method)
    (invalidate-caches generic)
    (old-add-method generic method)))

(define (cacheable-generic-function? closure)
  (and (generic-function? closure)
       (not (member? closure generic-invocation-generics))))

(define (compute-equiv-function generic methods)
  "compute the equivalent method for a given generic and set of
applicable methods"
  (let* ((procs (map method-procedure methods))
	 (args nil) ;; not thread safe!
	 (bound (reduce (lambda (next-proc proc)
			  (lambda ()
			    (apply proc (cons next-proc args))))
			(reverse procs)
			(lambda ()
			  (throw-error "No applicable methods/next methods.")))))
    (lambda (real-args)
      (set! args real-args)
      (bound))))

(define (rewrite-generic-closure-callsite exp)
  "given the generic invocation EXP, memoize the results of
compute-methods for reuse between calls"
  (let ((cached-function (gensym))
	(cached-args-cls (gensym))
	(evald-args (gensym)))
    `(let-static ((,cached-function nil)
		  (,cached-args-cls nil)
		  (,evald-args nil))
       (set! ,evald-args (list ,@(rest exp)))

       (if (equal? ,cached-args-cls (mapr class-of ,evald-args))
	   (,cached-function ,evald-args) ;; hit cache

	   (begin
	     (set! ,cached-function
		   (compute-equiv-function ,(first exp)
					   ((compute-methods ,(first exp))
					    ,evald-args)))
	     (set! ,cached-args-cls (mapr class-of ,evald-args))
	     (apply add-invalidator (list ,(first exp)
	       (lambda () (set! ,cached-args-cls nil))))
	     
	     ;; do the call
	     (,cached-function ,evald-args))))))

;; temp turn it off again
;(define (cacheable-generic-function? closure) #f)

;
; All done.
;
;

'tiny-clos-up-and-running
