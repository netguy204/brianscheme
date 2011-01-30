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
  "List the current issues.")

(define (bs-new args)
  "Create a new issue.")
