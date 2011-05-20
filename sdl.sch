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
	(header "<SDL/SDL.h>"))

    (define-constant-function sdl:INIT-VIDEO
      (ffi:get-const header "%d" "SDL_INIT_VIDEO"))

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

    (define (sdl:event:empty)
      (let ((sz (sdl:size-of-event)))
	(ffi:set-bytes (ffi:make-bytes sz) 0 sz 0)))

    (define (sdl:event:unpack bytes offset)
      (let ((type (unpack-sdl:event-type bytes (+ offset (sdl:event:type-offset)))))
	(make-sdl:event 'type type)))

    (define (sdl:poll-event)
      (let* ((ev (sdl:event:empty))
	     (res (ffi:funcall poll-event 'ffi-uint ev)))
	(if (= res 1)
	    (sdl:event:unpack ev 0)
	    nil)))

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
      (ffi:funcall quit 'ffi-void))
))


(define (sdl:test:safe-poll)
  (guard
   (ex (#t (printf "got exception: %a\n" ex)
	   nil))

   (sdl:poll-event)))

(define (sdl:test image)
  (sdl:init (sdl:INIT-VIDEO))
  (sdl:wm-set-caption "SDL Test" "SDL Test")
  (let* ((screen (sdl:set-video-mode 640 480 0 0))
	 (tmp (sdl:load-bmp image))
	 (bmp (sdl:display-format tmp)))
    (sdl:free-surface tmp)

    (let loop ((ev (sdl:test:safe-poll)))
      (when ev
	    (printf "got event %a\n" ev))
      (sdl:blit-surface bmp 0 screen 0)
      (sdl:update-rect screen 0 0 640 480)
      (loop (sdl:test:safe-poll)))))
