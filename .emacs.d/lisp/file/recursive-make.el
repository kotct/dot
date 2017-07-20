;;; THIS FILE IS NOT LOADED AT STARTUP.
;;; This file is autoloaded on the following symbols:
;;;  kotct/recursive-make

;;;###autoload
(defun kotct/recursive-make ()
  "Recursively search for a make function."
  (interactive)
  (if (get-buffer "*recursive-make*")
      (kill-buffer "*recursive-make*"))
  (start-process "recursive-make" "*recursive-make*"
                 "~/.emacs.d/python/findmake.py" (buffer-file-name (current-buffer)))
  (display-buffer "*recursive-make*"))

(provide 'recursive-make)
