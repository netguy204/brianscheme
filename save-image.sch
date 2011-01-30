(if (save-image (second *args*))
    (display "Image saved.\n")
    (display "Failed to save image.\n"))
(exit 0)
