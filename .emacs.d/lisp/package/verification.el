(require 'cl-lib)

(require 'dependencies)
(require 'packup)

;; check to make sure all dependencies are installed before continuing
(defun kotct/check-dependency-list (&optional frame)
  "Check to make sure all dependencies in `kotct/dependency-list' are installed.
If not, interactively prompt the user to install them via `kotct/packup-install-dependencies'.

FRAME is used to determine whether or not we are running headless
(e.g., as a result of `emacs --daemon`). If we are, throw 'daemon-mode.
It is the caller's responsibility to catch this and handle it properly.
Typically this happens in init.el."
  (if (not (cl-every #'package-installed-p kotct/dependency-list))
      (if (string= (terminal-name (frame-terminal frame)) "initial_terminal")
          (throw 'daemon-mode 'daemon-mode)
        (y-or-n-p "You don't seem to have all necessary packages installed. Install them?\n(This configuration will probably not work if you don't.)")
        ;; since loaddefs don't get evaluated until after init, we must
        ;; manually require packup (normally loaded via autoloads)
        (require 'packup)
        (kotct/packup-install-dependencies nil))))

;; provide before checking, so that if checking fails (i.e. we're in
;; daemon mode) we know that we have the feature
(provide 'verification)

(kotct/check-dependency-list)
