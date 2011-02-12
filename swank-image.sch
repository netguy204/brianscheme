;; Save a standalone Swank image

(require 'swank)
(save-image "swank" 'executable #t 'toplevel swank-listen 'compress #t)
