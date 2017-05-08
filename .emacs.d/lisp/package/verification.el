(require 'cl)

;; check to make sure all dependencies are installed before continuing
(defun kotct/check-dependency-list (&optional frame)
  ;; (if frame (error "%s" frame))
  (if (not (every #'package-installed-p kotct/dependency-list))
      (if (string= (terminal-name (or frame (frame-terminal))) "initial_terminal")
          (throw 'daemon-mode 'daemon-mode)
        (y-or-n-p "You don't seem to have all necessary packages installed. Install them?\n(Your emacs will probably not work if you don't.)")
        ;; since loaddefs don't get evaluated until after init, we must
        ;; manually require packup (normally loaded via autoloads)
        (require 'packup)
        (kotct/packup-install-dependencies nil))))

;; provide before checking, so that if checking fails (i.e. we're in
;; daemon mode) we know that we have the feature
(provide 'verification)

(kotct/check-dependency-list)
