(require 'sdl)

(define-generic make-compatible-surface
  "internal method. creates an sdl surface that's compatible with a
given canvas")

(define-generic draw
  "request that a morph update its canvas")

(define-generic visible?
  "true if a morph is visible")

(define-generic make-compatible-canvas
  "create a canvas that's compatible with the supplied canvas")

(define-generic width
  "width of an object")

(define-generic height
  "height of an object")

(define-generic position-x
  "relative position of an object")

(define-generic position-y
  "relative positioin of an object")

(define-generic contains?
  "#t if the second item is fully contained by the first")

(define-generic bounds
  "the bounds of an object")

(define-generic draw-box
  "draw a filled box on a canvas")

(define-generic draw-canvas
  "draw a canvas onto another canvas at bounds")

(define-generic add-child
  "add a morph to another as a child")

(define-generic handle-mouse-motion
  "called when mouse motion occurs inside a morph")

(define-generic handle-mouse-down
  "called when a button is clicked inside a morph")

(define-generic handle-mouse-up
  "called when a button is released inside a morph")

(define-class <canvas> ()
  "something you can draw on"
  ('surface
   'width
   'height))

(define-method (make-compatible-surface (canvas <canvas>) width height)
  (sdl:create-compatible-surface (slot-ref canvas 'surface)
					    width
					    height))

(define-method (make-compatible-canvas (canvas <canvas>) width height)
  (make <canvas>
    'surface (make-compatible-surface canvas width height)
    'width width
    'height height))

(define-method (width (canvas <canvas>))
  (slot-ref canvas 'width))

(define-method (height (canvas <canvas>))
  (slot-ref canvas 'height))

(define (init-canvas canvas width height template-or-actual template?)
  "initialize CANVAS by building from a template or by wrapping a
primitive surface"
  (assert (and width height))
  (slot-set! canvas 'width width)
  (slot-set! canvas 'height height)

  (if template?
      (if template-or-actual
	  ;; base ourselves off the template
	  (slot-set! canvas 'surface
		     (make-compatible-surface template-or-actual width height))
	  ;; use the global world as a template
	  (slot-set! canvas 'surface
		     (make-compatible-surface *world* width height)))
      ;; wrap a primitive surface
      (slot-set! canvas 'surface (assert template-or-actual))))

(define-method (initialize (canvas <canvas>) args)
  (let ((width (getl args 'width nil))
	(height (getl args 'height nil))
	(surface (getl args 'surface nil)))

    (if surface
	(init-canvas canvas width height surface #f)
	(init-canvas canvas width height
		     (getl args 'template nil)
		     #t))))

(define-class <world> (<canvas>)
  "the root level canvas"
  ('morphs))

(define (create-world title width height)
  "create a new world canvas"
  (sdl:init (sdl:INIT-VIDEO))
  (sdl:wm-set-caption title title)
  (let ((screen (sdl:set-video-mode width height 0 0)))
    (set! *world* (make <world>
		    'surface screen
		    'width width
		    'height height))))

(define-method (add-child (world <world>) morph)
  (slot-set! world 'morphs
	     (cons morph (slot-ref world 'morphs)))
  (slot-set! morph 'parent world))

(define-method (draw (world <world>))
  (dolist (morph (slot-ref world 'morphs))
    (when (and (slot-ref morph 'dirty))
	  (draw morph))

    (when (visible? morph)
	  (draw-canvas world morph
		       (slot-ref morph 'bounds))))

  (sdl:update-rect (slot-ref world 'surface) 0 0
		   (width world) (height world)))

(define-class <bounds> ()
  "defines general boundries"
  ('w 'h 'x 'y))

(define-method (print-object (strm <output-stream>)
			     (bounds <bounds>))
  (ssprintf strm "x: %a y: %a w: %a h: %a"
	    (position-x bounds)
	    (position-y bounds)
	    (width bounds)
	    (height bounds)))

(define-method (width (bounds <bounds>))
  (slot-ref bounds 'w))

(define-method (height (bounds <bounds>))
  (slot-ref bounds 'h))

(define-method (position-x (bounds <bounds>))
  (slot-ref bounds 'x))

(define-method (position-y (bounds <bounds>))
  (slot-ref bounds 'y))

(define-class <morph> (<canvas>)
  "a dynamic nestable entity in the world"
  ('bounds
   'dirty
   'parent
   'children
   'visible))

(define-method (initialize (morph <morph>) args)
  (let ((parent (getl args 'parent nil))
	(bounds (getl args 'bounds nil)))

    ;; if parent isn't explicit then we assume world
    (unless parent
      (set! parent *world*))

    (add-child parent morph)

    (slot-set! morph 'bounds bounds)
    (slot-set! morph 'dirty #t)
    (slot-set! morph 'children nil)

    (init-canvas morph (width bounds) (height bounds) parent #t)))

(define-method (handle-mouse-motion (morph <morph>) position click)
  nil)

(define-method (handle-mouse-down (morph <morph>) position button)
  nil)

(define-method (handle-mouse-up (morph <morph>) position button)
  nil)

(define (absolute-bounds outer inner)
  "return INNER in absolute terms instead of relative to OUTER"
  (let ((x0 (position-x outer))
	(y0 (position-y outer))
	(x1 (position-x inner))
	(y1 (position-y inner))
	(w (width inner))
	(h (height inner)))
    (make <bounds>
      'x (+ x0 x1)
      'y (+ y0 y1)
      'w w 'h h)))

(define-method (contains? (bounds <bounds>) (test <bounds>))
  (let ((bx (position-x bounds))
	(by (position-y bounds))
	(bw (width bounds))
	(bh (height bounds))
	(tx (position-x test))
	(ty (position-y test))
	(tw (width test))
	(th (height test)))

    (and (>= tx bx)
	 (>= ty by)
	 (<= (+ tx tw) (+ bx bw))
	 (<= (+ ty th) (+ by bh)))))

(define-method (add-child (morph <morph>) other)
  (slot-set! morph 'children
	     (cons other (slot-ref morph 'children)))
  (slot-set! other 'parent morph)
  (slot-set! morph 'dirty #t))

(define-method (draw (morph <morph>))
  (dolist (m (slot-ref morph 'children))
    (when (slot-ref m 'dirty)
	  (draw m))

    (when (visible? m)
	  (draw-canvas morph m (bounds m)))

    (slot-set! m 'dirty #f)))

(define-method (bounds (morph <morph>))
  (slot-ref morph 'bounds))

(define-method (visible? (morph <morph>))
  (slot-ref morph 'visible))

(define-struct color
  (r g b a))

(define-method (print-object (stream <output-stream>)
			     (morph <morph>))
  (ssprintf stream "#<instance-of %a bounds: (%a) children: %a>"
	    (class-of morph)
	    (bounds morph)
	    (slot-ref morph 'children)))

(define-class <box-morph> (<morph>)
  "a very simple morph"
  ('color))

(define-method (initialize (box <box-morph>) args)
  (slot-set! box 'color (getl args 'color nil))
  (slot-set! box 'visible #t)
  (call-next-method))

(define-method (draw (box <box-morph>))
  (let* ((b (bounds box))
	 (w (width b))
	 (h (height b)))
    (draw-box box (make <bounds>
		    'x 0 'y 0
		    'w w 'h h)
	      (slot-ref box 'color)))
  (call-next-method))

(define-class <hand-morph> (<morph>)
  "represents the users pointer"
  ())

(define-method (initialize (hand <hand-morph>) args)
  (let ((pos (getl args 'position '(0 0))))
    (slot-set! hand 'bounds
	       (make <bounds>
		 'x (first pos)
		 'y (second pos)
		 'w 0
		 'h 0))

    (slot-set! hand 'visible #f)
    (slot-set! hand 'dirty #f)
    (add-child *world* hand)))

(define-method (handle-mouse-motion (hand <hand-morph>) position click)
  (let ((b (bounds hand)))
    (slot-set! b 'x (first position))
    (slot-set! b 'y (second position))))

(define (handle-events world hand)
  "this method loops until world terminates"
  (let ((ev (sdl:wait-event)))
    (case (and ev (sdl:event-type-ref ev))
      (sdl:mouse-motion
       (let ((v (sdl:event-value-ref ev)))
	 (handle-mouse-motion hand
			      (list (second (assoc 'x v))
				    (second (assoc 'y v)))
			      (> (second (assoc 'state v)) 0)))))

    (draw world)
    (handle-events world hand)))

;; canvas drawing commands

(define-method (draw-box (canvas <canvas>) bounds color)
  (let* ((x (slot-ref bounds 'x))
	 (y (slot-ref bounds 'y))
	 (w (slot-ref bounds 'w))
	 (h (slot-ref bounds 'h)))
    (sdl:box-rgba (slot-ref canvas 'surface) x y (+ x w) (+ y h)
		  (color-r-ref color)
		  (color-g-ref color)
		  (color-b-ref color)
		  (color-a-ref color))))

(define (bounds->rect bounds use-offset?)
  (if use-offset?
      (sdl:make-rect (position-x bounds) (position-y bounds)
		     (width bounds) (height bounds))
      (sdl:make-rect 0 0 (width bounds) (height bounds))))

(define-method (draw-canvas (canvas <canvas>) other bounds)
  (let ((src-rect (bounds->rect bounds #f))
	(dst-rect (bounds->rect bounds #t))
	(src-surf (slot-ref other 'surface))
	(dst-surf (slot-ref canvas 'surface)))

    (sdl:blit-surface src-surf src-rect dst-surf dst-rect)))

(define (morphic:test)
  (let* ((world (create-world "Morphic World!" 640 480))
	 (toss (printf "world!\n"))
	 (bounds (make <bounds>
		   'x 30
		   'y 30
		   'w 128
		   'h 160))
	 (toss (printf "bounds!\n"))
	 (morph (make <box-morph>
		  'parent world
		  'color (make-color 'r 128 'g 128 'b 255 'a 255)
		  'bounds bounds))
	 (toss (printf "box!\n"))

	 (morph2 (make <box-morph>
		   'parent morph
		   'color (make-color 'r 255 'g 128 'b 128 'a 255)
		   'bounds (make <bounds>
			     'x 2
			     'y 2
			     'w (- (width bounds) 4)
			     'h 30)))
	 (hand (make <hand-morph>)))

    (handle-events world hand)))

