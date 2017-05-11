(let ((dot-config (concat (file-name-directory (or (getenv "XDG_CONFIG_HOME") "~/.config/")) "dot/"))
      (emacs-subdir "emacs/"))
  (load (concat dot-config emacs-subdir "init") 'noerror nil nil 'must-suffix))

(provide 'machine-local)
