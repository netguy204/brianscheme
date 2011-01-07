; Copyright 2011 Christopher Wellons
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

; DESCRIPTION: Import of my Wisp (lisp) programming language.
; http://git.nullprogram.com/?p=wisp.git

; You'll probably need to define WISPROOT in your environment so that
; it can find the standard libraries.

(require 'ffi)

(let* ((wisp (ffi:dlopen "libwisp.so"))
       (init (ffi:dlsym wisp "wisp_init"))
       (repl (ffi:dlsym wisp "repl")))

  (define (wisp:init)
    "Initialize the Wisp library."
    (ffi:funcall init 'ffi-void))

  (define (wisp:repl)
    "Fire up a Wisp REPL."
    (ffi:funcall repl 'ffi-void)))

(wisp:init)
