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
; Provides a very primitive http server.
;
; (http-server 8080 test-handler)
;
; And open localhost:8080 in your browser.

(require 'string)

(define (socket-parse-line line)
  (let ((vals (string-split line #\:)))
    (list (first vals)
	  (trim (second vals)))))

(define (socket-read-line sock)
  (let ((buff (make-string-buffer)))
    (let loop ((next (socket-read sock 1)))
      (let ((char (string-ref (second next) 0)))
	(write-stream buff char)
	(if (eq? char #\newline)
	    (chomp (string-buffer->string buff))
	    (loop (socket-read sock 1)))))))

(define (http-server port handler)
  (let* ((server (make-server-socket port))
	 (conn (socket-accept server))
	 (req (socket-read-line conn))
	 (hdrs nil))
    (let loop ((data (socket-read-line conn)))
      (cond
       ((= (string-length data) 0)
	(handler conn (list req hdrs))
	(socket-close server))
       (else
	(push! (socket-parse-line data) hdrs)
	(loop (socket-read-line conn)))))))

(define (append-eol buff)
  (write-stream buff (integer->char 13))
  (write-stream buff #\newline))

(define (header-code code)
  (let ((buff (make-string-buffer)))
    (write-stream buff "HTTP/1.1 ")
    (write-stream buff (number->string code))
    (write-stream buff " OK")
    (append-eol buff)
    (string-buffer->string buff)))

(define (header-field name value)
  (let ((buff (make-string-buffer)))
    (write-stream buff name)
    (write-stream buff ": ")
    (write-stream buff value)
    (append-eol buff)
    (string-buffer->string buff)))

(define (with-basic-header data)
  (let ((buff (make-string-buffer)))
    (write-stream buff (header-code 200))
    (write-stream 
     buff
     (header-field "Content-Length"
		   (number->string (string-length data))))
    (write-stream buff
		  (header-field "Content-Type"
				"text/html; charset=UTF-8"))

    (append-eol buff)
    (write-stream buff data)
    (string-buffer->string buff)))

(define (test-handler conn hdrs)
  (printf "handler\n")
  (let ((data (with-basic-header
	       "<html><body><h1>Hello World!</h1></body></html>")))
    (printf "wrote: %a\n"
	    (socket-write conn data (string-length data)))
    (socket-close conn)))
