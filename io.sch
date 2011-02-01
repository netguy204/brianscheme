;;; Input/Output functions and macros

(define-syntax (with-open-file binding . body)
  (let ((var (first binding))
        (file (second binding))
        (res (gensym)))
    `(let ((,var (open-input-port ,file)))
       (if (eof-object? ,var)
           (throw-error "failed to open" ,file)
           (let ((,res (begin ,@body)))
             (close-input-port ,var)
             ,res)))))

(define (chmod file mode)
  "Change mode of filename in string FILE to MODE."
  (assert-types (file string?) (mode integer?))
  (%chmod file mode))

(define (umask mode)
  "Set the current process umask."
  (assert-types (mode integer?))
  (%umask mode))

(define (mkdir file . modearg)
  (let ((mode (car-else modearg 493)))
    (assert-types (file string?) (mode integer?))
    (%mkdir file mode)))

(define (rename-file oldname newname)
  "Rename/move a file."
  (assert-types (oldname string?) (newname string?))
  (%rename-file oldname newname))

(define (opendir dir)
  "Open a directory-stream for reading."
  (assert-types (dir string?))
  (%opendir dir))

(define (readdir stream)
  "Read the next entry in the directory stream."
  (assert-types (stream directory-stream?))
  (%readdir stream))

(define (closedir stream)
  "Close the directory stream."
  (assert-types (stream directory-stream?))
  (%closedir stream))

(define (dir name)
  "Return list of files in directory."
  (let ((dir (opendir name)))
    (letrec ((iter (lambda (file)
                      (if (eof-object? file)
                          '()
                          (if (or (equal? file ".")
                                  (equal? file ".."))
                              (iter (readdir dir))
                              (cons file (iter (readdir dir))))))))
      (let ((res (iter (readdir dir))))
        (closedir dir)
        res))))

(define (file-exists? name)
  "Return #t if file exists, otherwise false."
  (or (file-exists?0 name)
      (let ((dir (opendir name)))
        (if (eof-object? dir)
            #f
            (begin
              (closedir dir)
              #t)))))

(define (port-dump in out)
  "Quickly dump the contents of one port into the other port."
  (assert-types (in input-port?) (out output-port?))
  (%port-dump in out))

(define (open-input-pipe name)
  "Open a process for reading it's output (popen)."
  (assert-types (name string?))
  (%open-input-pipe name))

(define (open-output-pipe name)
  "Open a process to write to it (popen)."
  (assert-types (name string?))
  (%open-output-pipe name))

(define (read-line in)
  "Read the next line from the input port."
  (letrec ((iter (lambda (str next)
                    (if (or (eq? next #\newline) (eof-object? next))
                        str
                        (iter (string-append str (char->string next))
                              (read-char in))))))
    (iter "" (read-char in))))

(define (flush-output out)
  "Flush output port buffer."
  (assert-types (out output-port?))
  (%flush-output out))

(define (write-string str out)
  "Write a string to the output port."
  (dotimes (i (string-length str))
    (write-char (string-ref str i) out)))

(define (unread-char ch port)
  "Put a single character back into the read buffer."
  (assert-types (ch char?) (port input-port?))
  (%unread-char ch port))
