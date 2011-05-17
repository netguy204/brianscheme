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
	(header "<SDL/SDL.h>"))

    (define-constant-function sdl:INIT-VIDEO
      (ffi:get-const header "%d" "SDL_INIT_VIDEO"))

    (define-constant-function sdl:event:type-offset
      (ffi:offset-of header "SDL_Event" "type"))

    (define-constant-function sdl:size-of-event
      (ffi:size-of header "SDL_Event"))

    (define (sdl:event:empty)
      (let ((sz (sdl:size-of-event)))
	(ffi:set-bytes (ffi:make-bytes sz) 0 sz 0)))

    (define (sdl:event:unpack bytes offset)
      (let ((type (ffi:unpack-byte bytes (sdl:event:type-offset))))
	type))

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


(define (sdl:test image)
  (sdl:init (sdl:INIT-VIDEO))
  (sdl:wm-set-caption "SDL Test" "SDL Test")
  (let* ((screen (sdl:set-video-mode 640 480 0 0))
	 (tmp (sdl:load-bmp image))
	 (bmp (sdl:display-format tmp)))
    (sdl:free-surface tmp)
    (sdl:blit-surface bmp 0 screen 0)
    (sdl:update-rect screen 0 0 640 480)))
