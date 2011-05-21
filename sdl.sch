(require 'ffi)

(define-struct sdl:event
  (type
   value))

(with-library (handle "libSDL")
  (let ((init (ffi:dlsym handle "SDL_Init"))
	(set-caption (ffi:dlsym handle "SDL_WM_SetCaption"))
	(set-video-mode (ffi:dlsym handle "SDL_SetVideoMode"))
	(rw-from-file (ffi:dlsym handle "SDL_RWFromFile"))
	(load-bmp-rw (ffi:dlsym handle "SDL_LoadBMP_RW"))
	(display-format (ffi:dlsym handle "SDL_DisplayFormat"))
	(free-surface (ffi:dlsym handle "SDL_FreeSurface"))
	(upper-blit (ffi:dlsym handle "SDL_UpperBlit"))
	(update-rect (ffi:dlsym handle "SDL_UpdateRect"))
	(quit (ffi:dlsym handle "SDL_Quit"))
	(poll-event (ffi:dlsym handle "SDL_PollEvent"))
	(wait-event (ffi:dlsym handle "SDL_WaitEvent"))
	(create-rgb-surface (ffi:dlsym handle "SDL_CreateRGBSurface"))
	(header "<SDL/SDL.h>"))

    (define-constant-function sdl:INIT-VIDEO
      (ffi:get-const header "%d" "SDL_INIT_VIDEO"))

    (define-constant-function sdl:SW-SURFACE
      (ffi:get-const header "%d" "SDL_SWSURFACE"))

    (define-constant-function sdl:event:type-offset
      (ffi:offset-of header "SDL_Event" "type"))

    (define-constant-function sdl:size-of-event
      (ffi:size-of header "SDL_Event"))

    (ffi:define-enum "<SDL/SDL.h>" SDL_EventType sdl:event-type
       (SDL_NOEVENT sdl:no-event)          ; /**< Unused (do not remove) */
       (SDL_ACTIVEEVENT sdl:active-event)  ; /**< Application loses/gains visibility */
       (SDL_KEYDOWN sdl:key-down)          ; /**< Keys pressed */
       (SDL_KEYUP sdl:key-up)              ; /**< Keys released */
       (SDL_MOUSEMOTION sdl:mouse-motion)  ; /**< Mouse moved */
       (SDL_MOUSEBUTTONDOWN sdl:mouse-button-down) ;/**< Mouse button pressed */
       (SDL_MOUSEBUTTONUP sdl:mouse-button-up) ; /**< Mouse button released */
       (SDL_JOYAXISMOTION sdl:joy-axis-motion) ; /**< Joystick axis motion */
       (SDL_JOYBALLMOTION sdl:joy-ball-motion) ; /**< Joystick trackball motion */
       (SDL_JOYHATMOTION sdl:joy-hat-motion) ; /**< Joystick hat position change */
       (SDL_JOYBUTTONDOWN sdl:joy-button-down) ; /**< Joystick button pressed */
       (SDL_JOYBUTTONUP sdl:joy-button-up) ; /**< Joystick button released */
       (SDL_QUIT sdl:quit)                 ; /**< User-requested quit */
       (SDL_SYSWMEVENT sdl:sys-wm-event)   ; /**< System specific event */
       (SDL_VIDEORESIZE sdl:video-resize)  ; /**< User resized video mode */
       (SDL_VIDEOEXPOSE sdl:video-expose)  ; /**< Screen needs to be redrawn */
       (SDL_USEREVENT sdl:user-event))

    (define-constant-function sdl:rect:size-of
      (ffi:size-of header "SDL_Rect"))

    (define-constant-function sdl:rect:offset-of-x
      (ffi:offset-of header "SDL_Rect" "x"))

    (define-constant-function sdl:rect:offset-of-y
      (ffi:offset-of header "SDL_Rect" "y"))

    (define-constant-function sdl:rect:offset-of-w
      (ffi:offset-of header "SDL_Rect" "w"))

    (define-constant-function sdl:rect:offset-of-h
      (ffi:offset-of header "SDL_Rect" "h"))

    (define (sdl:make-rect x y width height)
      (let ((rect (ffi:make-bytes (sdl:rect:size-of))))
	(ffi:pack-bytes rect (sdl:rect:offset-of-x)
			(ffi:bs->machine (ffi:integer->bytes x 2)))
	(ffi:pack-bytes rect (sdl:rect:offset-of-y)
			(ffi:bs->machine (ffi:integer->bytes y 2)))
	(ffi:pack-bytes rect (sdl:rect:offset-of-w)
			(ffi:bs->machine (ffi:integer->bytes width 2)))
	(ffi:pack-bytes rect (sdl:rect:offset-of-h)
			(ffi:bs->machine (ffi:integer->bytes height 2)))

	rect))

    (define (sdl:event:empty)
      (let ((sz (sdl:size-of-event)))
	(ffi:set-bytes (ffi:make-bytes sz) 0 sz 0)))

    (ffi:define-header-struct "<SDL/SDL.h>" "SDL_Event"
      sdl:event:unpack-mouse-motion-event
      ("motion.state" state (ffi:make-int-unpacker 1))
      ("motion.x"     x     (ffi:make-int-unpacker 2))
      ("motion.y"     y     (ffi:make-int-unpacker 2))
      ("motion.xrel"  xrel  (ffi:make-int-unpacker 2))
      ("motion.yrel"  yrel  (ffi:make-int-unpacker 2)))

    (ffi:define-header-struct "<SDL/SDL.h>" "SDL_Event"
      sdl:event:unpack-mouse-button-event
      ("button.button" button (ffi:make-int-unpacker 1))
      ("button.state"  state  (ffi:make-int-unpacker 1))
      ("button.x"      x      (ffi:make-int-unpacker 2))
      ("button.y"      y      (ffi:make-int-unpacker 2)))

    (ffi:define-header-struct "<SDL/SDL.h>" "SDL_PixelFormat"
      sdl:unpack-pixel-format
      ("BitsPerPixel"  bits-per-pixel  (ffi:make-int-unpacker 1))
      ("BytesPerPixel" bytes-per-pixel (ffi:make-int-unpacker 1))
      ("Rmask"         rmask           (ffi:make-int-unpacker 4))
      ("Gmask"         gmask           (ffi:make-int-unpacker 4))
      ("Bmask"         bmask           (ffi:make-int-unpacker 4))
      ("Amask"         amask           (ffi:make-int-unpacker 4))
      ("alpha"         alpha           (ffi:make-int-unpacker 1)))

    (ffi:define-header-struct "<SDL/SDL.h>" "SDL_Surface"
      sdl:unpack-surface
      ("w"      w      (ffi:make-int-unpacker 4))
      ("h"      h      (ffi:make-int-unpacker 4))
      ("pitch"  pitch  (ffi:make-int-unpacker 2))
      ("format" format (lambda (bytes offset)
			 (sdl:unpack-pixel-format (ffi:unpack-pointer bytes offset) 0))))

    (ffi:define-header-struct "<SDL/SDL.h>" "SDL_Rect"
      sdl:unpack-rect
      ("x"      x      (ffi:make-int-unpacker 2))
      ("y"      y      (ffi:make-int-unpacker 2))
      ("w"      w      (ffi:make-int-unpacker 2))
      ("h"      h      (ffi:make-int-unpacker 2)))

    (define (sdl:event:unpack bytes offset)
      (let ((type (unpack-sdl:event-type bytes (+ offset (sdl:event:type-offset)))))
	(make-sdl:event 'type type
			'value (case type
				 ((sdl:mouse-button-down sdl:mouse-button-up)
				  (sdl:event:unpack-mouse-button-event bytes offset))
				 (sdl:mouse-motion
				  (sdl:event:unpack-mouse-motion-event bytes offset))
				 (else nil)))))

    (define (sdl:poll/wait-event which)
      (let* ((ev (sdl:event:empty))
	     (res (ffi:funcall which 'ffi-uint ev)))
	(if (= res 1)
	    (sdl:event:unpack ev 0)
	    nil)))

    (define (sdl:poll-event)
      (sdl:poll/wait-event poll-event))

    (define (sdl:wait-event)
      (sdl:poll/wait-event wait-event))

    (define (sdl:init flag)
      (ffi:funcall init 'ffi-uint flag))

    (define (sdl:wm-set-caption title iconified)
      (ffi:funcall set-caption 'ffi-void title iconified))

    (define (sdl:set-video-mode width height bpp flags)
      (ffi:funcall set-video-mode 'ffi-pointer width height bpp flags))

    (define (sdl:rw-from-file name mode)
      (ffi:funcall rw-from-file 'ffi-pointer name mode))

    (define (sdl:load-bmp-rw handle free?)
      (ffi:funcall load-bmp-rw 'ffi-pointer handle (if free? 1 0)))

    (define (sdl:load-bmp name)
      (sdl:load-bmp-rw (sdl:rw-from-file name "rb") 1))

    (define (sdl:display-format surface)
      (ffi:funcall display-format 'ffi-pointer surface))

    (define (sdl:create-rgb-surface flags width height depth rmask gmask bmask amask)
      (ffi:funcall create-rgb-surface 'ffi-pointer
		   flags width height depth rmask gmask bmask amask))

    (define (sdl:free-surface surface)
      (ffi:funcall free-surface 'ffi-void surface))

    (define (sdl:upper-blit src src-rect dst dst-rect)
      (ffi:funcall upper-blit 'ffi-uint src src-rect dst dst-rect))

    (define (sdl:blit-surface src src-rect dst dst-rect)
      ;; TODO: convenience rect syntax
      (sdl:upper-blit src src-rect dst dst-rect))

    (define (sdl:update-rect screen x y width height)
      (ffi:funcall update-rect 'ffi-void screen x y width height))

    (define (sdl:quit)
      (ffi:funcall quit 'ffi-void))))

(with-library (handle "libSDL_gfx")
  (let ((pixel-rgba (ffi:dlsym handle "pixelRGBA"))
	(line-rgba (ffi:dlsym handle "lineRGBA"))
	(aaline-rgba (ffi:dlsym handle "aalineRGBA"))
	(rectangle-rgba (ffi:dlsym handle "rectangleRGBA"))
	(box-rgba (ffi:dlsym handle "boxRGBA")))

    (define (sdl:pixel-rgba surface x y r g b a)
      (ffi:funcall pixel-rgba 'ffi-uint
		   surface
		   (ffi:alien-ushort x) (ffi:alien-ushort y)
		   (ffi:alien-uchar r) (ffi:alien-uchar g) (ffi:alien-uchar b)
		   (ffi:alien-uchar a)))

    (define (sdl:line-rgba surface x1 y1 x2 y2 r g b a)
      (ffi:funcall line-rgba 'ffi-uint
		   surface
		   (ffi:alien-ushort x1) (ffi:alien-ushort y1)
		   (ffi:alien-ushort x2) (ffi:alien-ushort y2)
		   (ffi:alien-uchar r) (ffi:alien-uchar g) (ffi:alien-uchar b)
		   (ffi:alien-uchar a)))

    (define (sdl:aaline-rgba surface x1 y1 x2 y2 r g b a)
      (ffi:funcall aaline-rgba 'ffi-uint
		   surface
		   (ffi:alien-ushort x1) (ffi:alien-ushort y1)
		   (ffi:alien-ushort x2) (ffi:alien-ushort y2)
		   (ffi:alien-uchar r) (ffi:alien-uchar g) (ffi:alien-uchar b)
		   (ffi:alien-uchar a)))

    (define (sdl:rectangle-rgba surface x1 y1 x2 y2 r g b a)
      (ffi:funcall rectangle-rgba 'ffi-uint
		   surface
		   (ffi:alien-ushort x1) (ffi:alien-ushort y1)
		   (ffi:alien-ushort x2) (ffi:alien-ushort y2)
		   (ffi:alien-uchar r) (ffi:alien-uchar g) (ffi:alien-uchar b)
		   (ffi:alien-uchar a)))

    (define (sdl:box-rgba surface x1 y1 x2 y2 r g b a)
      (ffi:funcall box-rgba 'ffi-uint
		   surface
		   (ffi:alien-ushort x1) (ffi:alien-ushort y1)
		   (ffi:alien-ushort x2) (ffi:alien-ushort y2)
		   (ffi:alien-uchar r) (ffi:alien-uchar g) (ffi:alien-uchar b)
		   (ffi:alien-uchar a)))))

(define (sdl:load-image-file file)
  "load image from FILE and convert to display format"
  (let* ((tmp (sdl:load-bmp file))
	 (img (sdl:display-format tmp)))
    (sdl:free-surface tmp)
    img))

(define (sdl:create-compatible-surface reference-surface width height)
  (let* ((surface (sdl:unpack-surface reference-surface 0))
	 (format (second (assoc 'format surface)))
	 (rmask (second (assoc 'rmask format)))
	 (gmask (second (assoc 'gmask format)))
	 (bmask (second (assoc 'bmask format)))
	 (amask (second (assoc 'amask format)))
	 (depth (second (assoc 'bits-per-pixel format))))

    (sdl:create-rgb-surface (sdl:SW-SURFACE) width height depth
			    rmask gmask bmask amask)))

(define (sdl:test)
  (sdl:init (sdl:INIT-VIDEO))
  (sdl:wm-set-caption "SDL Test" "SDL Test")
  (let* ((screen (sdl:set-video-mode 640 480 0 0))
	 (src-rect (sdl:make-rect 0 0 128 128))
	 (last-x nil)
	 (last-y nil)
	 (mouse-active #f))

    (set! *screen* screen)

    (let loop ((ev (sdl:wait-event)))
      (case (and ev (sdl:event-type-ref ev))
	(sdl:mouse-button-down (set! mouse-active #t)
			       (let ((v (sdl:event-value-ref ev)))
				 (set! last-x (second (assoc 'x v)))
				 (set! last-y (second (assoc 'y v)))))
	(sdl:mouse-button-up (set! mouse-active #f))
	(sdl:mouse-motion (when mouse-active
				(let* ((v (sdl:event-value-ref ev))
				       (x (second (assoc 'x v)))
				       (y (second (assoc 'y v))))
				  (sdl:aaline-rgba screen last-x last-y x y 255 255 255 255)
				  (set! last-x x)
				  (set! last-y y)))))

      ;(sdl:blit-surface bmp src-rect screen (sdl:make-rect bmp-x bmp-y 128 128))
      ;(sdl:pixel-rgba screen last-x last-y 255 255 255 255)
      (sdl:update-rect screen 0 0 640 480)

      (if (eq? (sdl:event-type-ref ev) 'sdl:quit)
	  (sdl:quit)
	  (loop (sdl:wait-event))))))
