;;; bullshit! (bs) --- a stupid issue tracker

;; Design:
;;
;;     This will be heavily geared at Git. Like Git, bs itself does
;;  not directly interact with the user. Commands are generally
;;  non-interactive transaction on the issue database, and in the
;;  event interaction is needed (entering detailed issue notes) the
;;  EDITOR is displayed.
;;
;;     Issues are identified by a unique ID (not a hash). Like a Git
;;  hash, they can be referenced by the substring. They are stored as
;;  a specially-printed plist in a flat-file database.

;; bs init
;;    Create an empty issue database.
;;
;; bs list
;;    List all open issues.
;;
;; bs show ISSUE
;;    Show all details on given issue.
;;
;; bs new MESSAGE
;;    Just like create, but immediately commit the issue into Git.
;;
;; bs close ISSUE
;;    Set an issue to closed.

;;     There will be an option switch for all commands to immediately
;;  commit the database change into Git.

(define bs-dir ".bs"
  "Location of issue database.")

(define (bs-exec cmd args)
  "Execute user command."
  (cond
   ((equal? cmd "help") (bs-help args))
   ((equal? cmd "init") (bs-init args))
   ((equal? cmd "list") (bs-list args))
   ((equal? cmd "new")  (bs-new  args))
   ((eq? cmd nil) (bs-help args))
   (#t (begin
         (display "Unknown command ")
         (display cmd)))))

(define (bs-init args)
  "Initialize bs database in current directory."
  (if (mkdir bs-dir)
      (display "Initialized empty issue database.\n")
      (display "Failed to initialize database.\n")))

(define (bs-help args)
  (display "usage: bs [bs-opts] command [cmd-opts]\n\n")
  (display "help    Print this help information\n")
  (display "init    Create an empty issue database\n")
  (display "list    Print list of current issues.")
  (display "new     Create a new issue.")
  (display "\n"))

(define (bs-list args)
  "List the current issues."
  (dolist (issue (dir bs-dir))
    (print-issue-short (fetch-issue issue))))

(define (bs-new args)
  "Create a new issue."
  (let ((id (number->string (create-id))))
    (write-issue (list 'id id 'user "user" 'title (car args) 'status 'open))
    (display (string-append "Created issue " id "\n"))))

(define (fetch-issue name)
  "Fetch an issue s-exp by name."
  (let ((port (open-input-port (string-append bs-dir "/" name))))
    (if (eof-object? port)
        '()
        (read-port port))))

(define (print-issue-short issue)
  "Print out the issue summary in one line."
  (when (eq? (plist-get issue 'status) 'open)
    (display (plist-get issue 'id))
    (display "    ")
    (display (plist-get issue 'user))
    (display "    ")
    (display (plist-get issue 'title))
    (display "\n")))

(define (write-issue issue)
  "Write the given issue to the database."
  (let ((port (open-output-port
               (string-append bs-dir "/" (plist-get issue 'id)))))
    (write-port issue port)
    (close-output-port port)))

(define (create-id)
  "Create a new issue id."
  (let ((id (mod (abs (make-seed)) 10000)))
    (if (< id 1000)
        (create-id)
        id)))
