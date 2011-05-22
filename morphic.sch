(require 'sdl)

(define-generic make-compatible-surface
  "internal method. creates an sdl surface that's compatible with a
given canvas")

(define-generic draw
  "request that a morph update its canvas")

(define-generic draw-on
  "draw an object onto a canvas")

(define-generic visible?
  "true if a morph is visible")

(define-generic dirty?
  "true if a morph needs to be redrawn")

(define-generic set-dirty!
  "mark a morph that needs to be redrawn")

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
  ('surface))

(define-method (initialize (canvas <canvas>) args)
  (let ((surface (assert (getl args 'surface nil))))
    (slot-set! canvas 'surface surface)))

(define (canvas->surface-info canvas)
  (sdl:unpack-surface (slot-ref canvas 'surface) 0))

(define-method (width (canvas <canvas>))
  (second (assoc 'w (canvas->surface-info canvas))))

(define-method (height (canvas <canvas>))
  (second (assoc 'h (canvas->surface-info canvas))))

(define-method (make-compatible-canvas (surface <alien>) width height)
  (make <canvas>
    'surface (sdl:create-compatible-surface surface width height)))

(define-method (make-compatible-canvas (canvas <canvas>) width height)
  (make-compatible-canvas (slot-ref canvas 'surface) width height))


(define-class <world> (<canvas>)
  "the root level canvas"
  ('morphs))

(define-method (initialize (world <world>) args)
  (slot-set! world 'morphs nil)
  (call-next-method))

(define (create-world title width height)
  "create a new world canvas"
  (sdl:init (sdl:INIT-VIDEO))
  (sdl:wm-set-caption title title)
  (let ((screen (sdl:set-video-mode width height 0 0)))
    (set! *world* (make <world>
		    'surface screen))))

(define-method (add-child (world <world>) morph)
  (slot-set! world 'morphs
	     (cons morph (slot-ref world 'morphs)))
  (slot-set! morph 'parent world))

(define-method (draw (world <world>))
  (dolist (morph (slot-ref world 'morphs))
    (when (visible? morph)
	  (draw-on world morph))

  (sdl:update-rect (slot-ref world 'surface) 0 0
		   (width world) (height world))))

(define-method (position-x (world <world>))
  0)

(define-method (position-y (world <world>))
  0)

(define-class <bounds> ()
  "defines general boundries"
  ('x 'y 'w 'h))

(define-method (initialize (b <bounds>) args)
  (slot-set! b 'x (getl args 'x 0))
  (slot-set! b 'y (getl args 'y 0))
  (slot-set! b 'w (getl args 'w 0))
  (slot-set! b 'h (getl args 'h 0)))

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

(define-class <morph> (<bounds>)
  "a dynamic nestable entity in the world"
  ('parent
   'children))

(define-method (initialize (morph <morph>) args)
  (let ((parent (getl args 'parent nil)))

    (when parent
	  (add-child parent morph))

    (call-next-method)))

(define-method (handle-mouse-motion (morph <morph>) position click)
  nil)

(define-method (handle-mouse-down (morph <morph>) position button)
  nil)

(define-method (handle-mouse-up (morph <morph>) position button)
  nil)

(define-method (visible? (morph <morph>))
  #f)

(define-method (add-child (morph <morph>) other)
  (slot-set! morph 'children
	     (cons other (slot-ref morph 'children)))
  (slot-set! other 'parent morph))

(define-method (draw-on (canvas <canvas>) (morph <morph>))
  nil)

(define-method (dirty? (morph <morph>))
  #f)

(define-method (print-object (stream <output-stream>)
			     (morph <morph>))
  (ssprintf stream "#<instance-of %a x: %a y: %a children: %a>"
	    (class-of morph)
	    (slot-ref morph 'x)
	    (slot-ref morph 'y)
	    (slot-ref morph 'children)))

(define-class <canvas-morph> (<morph>)
  "a morph that draws to an off-screen canvas"
  ('dirty
   'canvas))

(define-method (initialize (cm <canvas-morph>) args)
  (call-next-method)
  (let ((w (width cm))
	(h (height cm)))
    (slot-set! cm 'canvas (make-compatible-canvas *world* w h))
    (set-dirty! cm #t)))

(define-method (add-child (cm <canvas-morph>) other)
  (set-dirty! cm #t)
  (call-next-method))

(define-method (visible? (cm <canvas-morph>))
  #t)

(define-method (dirty? (cm <canvas-morph>))
  (slot-ref cm 'dirty))

(define-method (set-dirty! (cm <canvas-morph>) val)
  (slot-set! cm 'dirty val))

(define-method (draw-on (canvas <canvas>) (morph <canvas-morph>))
  (when (dirty? morph)
	(draw morph))
  (draw-canvas canvas (slot-ref morph 'canvas) morph))

(define-method (draw (cm <canvas-morph>))
  (dolist (sub (slot-ref cm 'children))
	  (draw-on (slot-ref cm 'canvas) sub))
  (set-dirty! cm #f))

(define-struct color
  (r g b a))

(define-class <box-morph> (<canvas-morph>)
  "a very simple morph"
  ('color))

(define-method (initialize (box <box-morph>) args)
  (slot-set! box 'color (getl args 'color nil))
  (call-next-method))

(define-method (draw (box <box-morph>))
  (let* ((w (width box))
	 (h (height box)))
    (draw-box (slot-ref box 'canvas)
	      (slot-ref box 'color)
	      0 0 w h))
  (call-next-method))

(define-class <hand-morph> (<morph>)
  "represents the user's pointer"
  ())

(define-method (handle-mouse-motion (hand <hand-morph>) position click)
  (slot-set! hand 'x (first position))
  (slot-set! hand 'y (second position)))

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

(define-method (draw-box (canvas <canvas>) color x y w h)
  (sdl:box-rgba (slot-ref canvas 'surface) x y (+ x w) (+ y h)
		(color-r-ref color)
		(color-g-ref color)
		(color-b-ref color)
		(color-a-ref color)))

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

	 (morph (make <box-morph>
		  'parent world
		  'color (make-color 'r 128 'g 128 'b 255 'a 255)
		  'x 30
		  'y 30
		  'w 128
		  'h 160))
	 (toss (printf "box!\n"))

	 (morph2 (make <box-morph>
		   'parent morph
		   'color (make-color 'r 255 'g 128 'b 128 'a 255)
		   'x 2
		   'y 2
		   'w (- (width morph) 4)
		   'h 30))

	 (toss (printf "another box!\n"))

	 (hand (make <hand-morph>
		 'parent world))
	 
	 (toss (printf "hand!\n")))

    (handle-events world hand)))

