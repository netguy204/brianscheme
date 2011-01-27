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
; A simple exception/condition system

;; this is the dynamic binding that holds the information for
;; resolving the current handler of signals

;;
;; the base case is a hard exit, the repl will install something more
;; friendly
;;
(defvar *condition-handler*
  (lambda (exception)
    (write-port "unhandled condition: " stderr)
    (write-with-spaces stderr exception)
    (exit 1)))

(define (raise obj)
  "throw object up to the currently installed *condition-handler*"
  ((*condition-handler*) obj))

(define (with-exception-handler handler thunk)
  (let* ((error-cont nil)
	 (restart (call/cc (lambda (cc)
			     (set! error-cont cc)
			     #f))))

    (if restart
	(handler (cdr restart))
	(binding ((*condition-handler* (lambda (ex)
					 (error-cont (cons #t ex)))))
          (thunk)))))

;; replace error and throw error with exception raising equivalents
(define (error . objs)
  "raise an error-condition"
  (raise (cons 'error-condition objs)))

(define throw-error error)

