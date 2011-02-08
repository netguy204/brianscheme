;; Builds a standalone bs executable

(require 'bs-lib)
(save-image "bs" 'executable #t 'toplevel process-args-img 'compress #t)
