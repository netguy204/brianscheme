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

; DESCRIPTION: Provides functions for dealing with saved images.

(define (save-image file (executable #f) (toplevel repl-or-script))
  "Save image to FILE. If given a second argument, run that on load."
  (assert-types (file string?))
  (define *after-image-start* toplevel)
  (%save-image file)
  (if executable
      (create-exec-image file)))

(define (create-exec-image file)
  "Turn the saved image FILE into a standalone executable."
  (let ((outfile (string-append file "~")))
    (letrec ((exe (open-input-port (find-library "bschsfx")))
             (img (open-input-port file))
             (out (open-output-port outfile))
             (iter (lambda (in char)
                     (unless (eof-object? char)
                       (write-char char out)
                       (iter in (read-char in))))))
      (iter exe (read-char exe))
      (iter img (read-char img))
      (close-input-port exe)
      (close-input-port img)
      (close-output-port out)
      (chmod outfile 493)
      (rename-file outfile file))))

;; this list of hooks will be executed before the user image load
;; handler is run
(set! *load-hooks* nil)

(define (*image-start*)
  (dolist (hook *load-hooks*)
    (hook))

  (*after-image-start*))
