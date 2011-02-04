(define (bs-file? file)
  "Is this a source file?"
  (and (> (string-length file) 4)
       (equal? (substring file (- (string-length file) 4)) ".sch")))

(define (compare-read-ports old new (success #t))
  (let ((old-in (old-read old))
        (new-in (read new)))
    (unless (equal? old-in new-in)
      (display "Reader mismatch:\n")
      (display old-in)
      (newline)
      (display new-in)
      (newline)
      (set! success #f))
    (if (or (eof-object? old-in) (eof-object? new-in))
        success
        (compare-read-ports old new 'success success))))

(define (compare-read file)
  "Compare old and new reader on given file."
  (if (compare-read-ports (open-input-port file) (open-input-port file))
      (begin (printf "Success for %s\n" file) #t)
      (begin (printf "Failure for %s\n" file) #f)))

(define (read-test-all)
  "Run test on entire BS source."
  (reduce (lambda (a b) (and a b))
          (map compare-read (filter bs-file? (dir ".")))))

(read-test-all)
