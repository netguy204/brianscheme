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
;;
;; bs commit MSG
;;    Commit all database changes into repository.

;;     By default almost all commands will immediately commit the
;;  database change into Git. There will be an option switch to not do
;;  this.

(define bs-dir ".bs"
  "Location of issue database.")

(define bs-config (string-append (or (getenv "HOME") "") "/.bsconfig"))

(define default-msg "Update bs database.")

(define (process-args-img)
  "Process arguments as an image."
  (bs-exec (first *args*) (cdr *args*)))

(define (process-args)
  "Process arguments as a script."
  (bs-exec (second *args*) (cddr *args*)))

;; Command processing

(define (bs-exec cmd args)
  "Execute user command."
  (load-config)
  (cond
   ((equal? cmd "help")   (bs-help   args))
   ((equal? cmd "init")   (bs-init   args))
   ((equal? cmd "list")   (bs-list   args))
   ((equal? cmd "new")    (bs-new    args))
   ((equal? cmd "commit") (bs-commit args))
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
  (display "list    Print list of current issues.\n")
  (display "new     Create a new issue.\n")
  (display "commit  Commit database to Git.\n"))

(define (bs-list args)
  "List the current issues."
  (dolist (issue (dir bs-dir))
    (print-issue-short (fetch-issue issue))))

(define (bs-new args)
  "Create a new issue."
  (let ((id (number->string (create-id))))
    (write-issue (list 'id id 'user *full* 'title (car args) 'status 'open))
    (display (string-append "Created issue " id "\n"))))

(define (bs-commit args)
  "Commit current database to the repository."
  (commit (car-else args default-msg)))

;; Issue handling

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
    (display "  ")
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

;; Committing

(define (string-prot s)
  "Protect string for use in the shell."
  (string-append "\"" s "\""))

(define (argcat . args)
  "Concatenate strings for use in a command line."
  (or (reduce (lambda (a b) (string-append a " " b)) (map string-prot args))
      ""))

(define-syntax (git cmd . args)
  "Run a git command."
  `(system (string-append "git " ,(symbol->string cmd) " "
                          (argcat ,@args) " > /dev/null")))

(define (commit msg)
  "Commit all current changes into the git repository."
  (unless (and (git reset)
               (git add bs-dir)
               (git commit "-qm" msg))
    (display "No database changes to commit.\n")))

;; Environment

(define (load-config)
  "Load configuration into global variables."
  (let ((config (get-config)))
    (define *user* (plist-get config 'user))
    (define *email* (plist-get config 'email))
    (define *full* (string-append *user* " <" *email* ">"))
    config))

(define (get-config)
  "Fetch configuration."
  (if (file-exists? bs-config)
      (call-with-input-file bs-config read-port)
      (get-git-config)))

(define (get-git-config)
  "Derive a config from Git."
  (let* ((name-in (open-input-pipe "git config user.name"))
         (mail-in (open-input-pipe "git config user.email"))
         (res (list 'user (read-line name-in) 'email (read-line mail-in))))
    (close-input-port name-in)
    (close-input-port mail-in)
    (if (or (= 0 (string-length (plist-get res 'user)))
            (= 0 (string-length (plist-get res 'email))))
        (list 'user "unknown" 'email "unknown")
        res)))