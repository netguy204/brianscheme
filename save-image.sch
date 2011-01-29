(if (save-image (second *args*))
    (display "Image saved.")
    (display "Failed to save image."))
(exit 0)
