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

(require 'getopt)
(require 'sugar)

(define *bs-dir* ".bs"
  "Location of issue database.")

(define (config-file)
  "Return location of global config file."
  (string-append (or (getenv "HOME") "") "/.bsconfig"))

(define (get-editor)
  "Return name of user's editor."
  (or (getenv "EDITOR") "nano"))

(define *default-msg* "[issue] Update bs database."
  "Default commit message.")

(define *tmp-file* ".git/BS_EDIT"
  "Temporary EDITOR file.")

(define *props* '(id priority status user title comments))

(define *priorities* '(low normal high urgent)
  "Priorities in order, 0-4.")

;; Error handling

(define (bs-error . msgs)
  "Produce an error message for the user."
  (display "bs: ")
  (dolist (msg msgs)
    (display msg))
  (newline)
  (exit 1))

;; Argument processing

(define (go-to-root)
  "Change current working directory to the project root."
  (cond
   ((file-exists? *bs-dir*) #t)
   ((equal? "/" (getcwd))
    (bs-error "Not in a project, or project uninitialized."))
   (#t (begin (chdir "..") (go-to-root)))))

(define (process-args-img)
  "Process arguments as an image."
  (define *argv* *args*)
  (bs-exec (first *args*) (cdr *args*)))

(define (process-args)
  "Process arguments as a script."
  (define *argv* (cdr *args*))
  (bs-exec (second *args*) (cddr *args*)))

(define (optlist opts)
  "Turn arguments into a plist."
  (letrec ((iter (lambda (opt)
                   (if opt
                       (append (list (string->symbol
                                      (char->string opt)) *optarg*)
                               (iter (getopt opts)))
                       '()))))
    (iter (getopt opts))))

;; Command processing

(define (bs-exec cmd args)
  "Execute user command."
  (load-config)
  (cond
   ((equal? cmd "help")   (bs-help   args))
   ((equal? cmd "init")   (bs-init   args))
   ((equal? cmd "list")   (bs-list   args))
   ((equal? cmd "show")   (bs-show   args))
   ((equal? cmd "new")    (bs-new    args))
   ((equal? cmd "commit") (bs-commit args))
   ((eq? cmd nil) (bs-help args))
   (#t (begin
         (display "Unknown command ")
         (display cmd)))))

(define (bs-init args)
  "Initialize bs database in current directory."
  (if (mkdir *bs-dir*)
      (display "Initialized empty issue database.\n")
      (display "Failed to initialize database.\n")))

(define (bs-help args)
  (display "usage: bs [bs-opts] command [cmd-opts]\n\n")
  (display "help    Print this help information\n")
  (display "init    Create an empty issue database\n")
  (display "list    Print list of current issues.\n")
  (display "show    Show all information on a commit.\n")
  (display "new     Create a new issue.\n")
  (display "commit  Commit database to Git.\n"))

(define (bs-list args)
  "List the current issues."
  (go-to-root)
  (dolist (issue (dir *bs-dir*))
    (print-issue-short (fetch-issue issue))))

(define (bs-show args)
  "Show all information on a commit."
  (if (null? args)
      (bs-error "must provide a commit to show"))
  (print-issue (fetch-issue (canon (car args)))))

(define (bs-new args)
  "Create a new issue."
  (go-to-root)
  (let* ((opts (optlist "np:t:"))
         (do-commit (not (plist-get opts 'n)))
         (id (create-id))
         (priority (priority (or (plist-get opts 'p) 'normal)))
         (title (or (plist-get opts 't) (edit-message)))
         (message (if (plist-get opts 't) '() (list (get-message))))
         (issue (list 'id id 'priority priority 'user *full*
                      'title title 'status 'open 'comments message)))
    (write-issue issue)
    (print-issue-short issue)
    (display (string-append "Created issue " id "\n"))
    (if do-commit
        (commit (string-append "[issue] " title)))))

(define (bs-commit args)
  "Commit current database to the repository."
  (go-to-root)
  (commit (car-else args *default-msg*)))

;; Issue handling

(define (canon short)
  "Find the full issue name for a possible short-hand name."
  (letrec ((match (lambda (lst)
                    (if (null? lst)
                        (bs-error "unknown issue: " short)
                        (if (equal? short (substring (car lst) 0
                                                     (string-length short)))
                            (car lst)
                            (match (cdr lst)))))))
    (match (dir *bs-dir*))))

(define (fetch-issue name)
  "Fetch an issue s-exp by name."
  (let ((file (string-append *bs-dir* "/" name)))
    (if (file-exists? file)
        (call-with-input-file file read-port)
        (bs-error "No such issue: " name))))

(define (print-issue-short issue)
  "Print out the issue summary in one line."
  (when (eq? (plist-get issue 'status) 'open)
    (display (substring (plist-get issue 'id) 0 7))
    (display "  ")
    (display (plist-get issue 'priority))
    (display "  ")
    (display (plist-get issue 'title))
    (display "\n")))

(define (print-issue issue)
  "Print out the issue summary in one line."
  (printf "issue %s    %a  (%a priority)\n"
          (plist-get issue 'id)
          (plist-get issue 'status)
          (plist-get issue 'priority))
  (printf "Author: %s\n" (plist-get issue 'user))
  (printf "\n\t%s\n\n" (plist-get issue 'title))
  (dolist (msg (plist-get issue 'comments))
    (display msg)
    (newline)))

(define (write-issue issue)
  "Write the given issue to the database."
  (let ((port (open-output-port
               (string-append *bs-dir* "/" (plist-get issue 'id)))))
    (write-string "(" port)
    (dolist (prop *props*)
      (write-port prop port)
      (write-char #\tab port)
      (write-port (plist-get issue prop) port)
      (write-char #\newline port))
    (write-string ")\n" port)
    (close-output-port port)))

(define (create-id)
  "Create a new issue id."
  (let ((pad (lambda (str)
               (if (< (string-length str) 4)
                   (string-append (make-string (- 4 (string-length str)) #\0)
                                  str)
                   str))))
    (apply string-append
           (map (compose pad (rcurry integer->string 'base 16) random)
                (make-list 4 (expt 2 16))))))

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
               (git add *bs-dir*)
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
  (if (file-exists? (config-file))
      (call-with-input-file (config-file) read-port)
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

(define (edit-message)
  "Summon the EDITOR to interact with the user."
  (close-output-port (open-output-port *tmp-file*))
  (let* ((ret (system (string-append (get-editor) " " *tmp-file*)))
         (title (call-with-input-file *tmp-file* read-line)))
    (unless ret
      (bs-error "editor aborted"))
    (if (= 0 (string-length title))
        (bs-error "empty message/title: aborting"))
    title))

(define (get-message)
  "Get the message from the tempfile, less the title."
  (call-with-input-file *tmp-file*
    [begin (read-line _) (read-line _) (slurp-port _)]))

;; Misc

(define (priority p)
  "Return the appropriate priority symbol."
  ;; Waiting on read-string to make this useful.
  (let ((p (if (string? p) (string->symbol p) p)))
    (cond
     ((null? p) (priority 1))
     ((and (number? p) (>= p 0) (<= p (length *priorities*)))
      (list-ref *priorities* p))
     ((and (symbol? p) (member? p *priorities*)) p)
     (#t (bs-error "unknown priority: " p)))))
