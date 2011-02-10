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
(require 'clos)

;;; CLOS stream

(define-class <socket-stream> (<output-stream> <input-stream>)
  "Apply stream concepts to sockets."
  ('conn
   'socket))

(define-method (read-stream-char (stream <socket-stream>))
  (let ((r (socket-read (slot-ref stream 'conn) 1)))
    (if (zero? (first r))
	*eof-object*
	(string-ref (second r) 0))))

(define-method (write-stream (stream <socket-stream>)
			     (string <string>))
  (socket-write (slot-ref stream 'conn) string (string-length string)))

(define (make-socket-stream conn)
  "Create a stream from an existing connection."
  (make <socket-stream> 'conn conn))

(define (make-server-stream port)
  "Create a stream TCP server, blocking on an incoming connection."
  (let ((server (make-server-socket port)))
    (make <socket-stream> 'conn (socket-accept server) 'socket server)))

;;; HTTP server

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
    (doto buff
      (write-stream "HTTP/1.1 ")
      (write-stream (number->string code))
      (write-stream " OK")
      (append-eol))
    (string-buffer->string buff)))

(define (header-field name value)
  (let ((buff (make-string-buffer)))
    (doto buff
      (write-stream name)
      (write-stream ": ")
      (write-stream value)
      (append-eol))
    (string-buffer->string buff)))

(define (with-basic-header data)
  (let ((buff (make-string-buffer)))
    (doto buff
      (write-stream (header-code 200))
      (write-stream
       (header-field "Content-Length"
		     (number->string (string-length data))))
      (write-stream
       (header-field "Content-Type"
		     "text/html; charset=UTF-8"))
      (append-eol)
      (write-stream data))
    (string-buffer->string buff)))

(define (test-handler conn hdrs)
  (printf "handler\n")
  (let ((data (with-basic-header
	       "<html><body><h1>Hello World!</h1></body></html>")))
    (printf "wrote: %a\n"
	    (socket-write conn data (string-length data)))
    (socket-close conn)))
