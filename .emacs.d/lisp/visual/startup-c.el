(setf inhibit-splash-screen t)


;;; prevent display of minibuffer message
(defun display-startup-echo-area-message ())


;;; scratch message
(defvar *inital-scratch-message-value* initial-scratch-message)

;; use a hook so that we can use emacs-init-time
(add-hook 'after-init-hook
          (lambda ()
            (if (equal initial-scratch-message *inital-scratch-message-value*)
                (setq initial-scratch-message
                      (concat ";; init: " (emacs-init-time) (format-time-string " @ %F %T") " (gc: " (format "%d" gcs-done) "x, " (format "%f" gc-elapsed) "s)"
                              "\n;; " (user-login-name) "@" (system-name) " loaded config for " kotct/user-current-username
                              "\n;; need help? try <C-h ?>"
                              "\n;; scratch away\n\n")))))

(provide 'startup-c)
