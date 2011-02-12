(if (save-image (second *args*) :compress #t)
    (display "Image saved.\n")
    (display "Failed to save image.\n"))
(exit 0)
