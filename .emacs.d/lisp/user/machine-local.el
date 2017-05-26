(defun kotct/dot-config-directory ()
  "Returns the directory name for where our machine-local Dot configuration directory should be."
  (concat (or (getenv "XDG_CONFIG_HOME") "~/.config/") "dot/"))

(defun kotct/dot-emacs-config-directory ()
  "Returns the directory name for where our machine-local Emacs subconfiguration directory should be."
  (let ((dot-config-dir (kotct/dot-config-directory))
        (emacs-subdir "emacs/"))
    (concat dot-config-dir emacs-subdir)))

(let ((emacs-config-directory (kotct/dot-emacs-config-directory)))
  (load (concat (file-name-directory emacs-config-directory) "init") 'noerror nil nil 'must-suffix))

(provide 'machine-local)
