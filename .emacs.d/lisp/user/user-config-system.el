;;; C-x C-z: switch personal configs

(defvar kotct/user-current-username
  nil
  "The username associated with the currently loaded personal config.")

(defun kotct/user-fetch-config (username)
  (message "fetching config for %s" username)
  (error "not gonna happen cause i suck"))

(defun kotct/user-get-default-username ()
  "Look up the default username set on this machine."
  (let ((filename "~/.emacs.d/lisp/user/default-username"))
    (if (file-exists-p filename)
        (with-temp-buffer
          (insert-file-contents filename)
          (replace-regexp-in-string "\n\\'" "" (buffer-string)))
      ;; default to base-config
      "base-config")))

(defun kotct/user-load-username (username)
  "Load the personal config of USERNAME.
Fetch the personal config from GitHub if it doesn't exist locally."
  (let ((personal-dir (concat "~/.emacs.d/lisp/user/users/" username)))
    (unless (file-exists-p personal-dir)
      (if (y-or-n-p (format "%s's config has not been fetched from GitHub. Fetch it now?" username))
          (kotct/user-fetch-config username)
        (error "No config for %s." username)))
    (add-to-list 'load-path personal-dir)
    (require (intern (concat username "-hub")))
    (setf kotct/user-current-username username)))

(defun kotct/user-unload-username (&optional username)
  "Unload the personal config of USERNAME, or if USERNAME is nil, `kotct/user-current-username'."
  (unless username (setf username kotct/user-current-username))
  (unload-feature (intern (concat username "-hub")) 'force))

(defun kotct/user-switch-username (&optional username)
  "Unload the current personal config and load USERNAME's personal config.
If USERNAME is nil, prompt for a username."
  (interactive)
  (unless username
    (setf username
          (ido-completing-read "Switch to username: "
                               (cddr (directory-files "~/.emacs.d/lisp/user/users/")))))
  (kotct/user-unload-username)
  ;; make sure that if the load fails we reload the stuff we just unloaded
  ;; so that we are actually loaded with current-username's config
  (condition-case err
      (kotct/user-load-username username)
    (error (kotct/user-load-username kotct/user-current-username)
           (signal (car err) (cdr err))))
  ;; maybe then also reload/rerun all the hooks and stuff for open buffers
  )

(global-set-key (kbd "C-x C-z") #'kotct/user-switch-username)

(kotct/user-load-username (kotct/user-get-default-username))

(provide 'user-config-system)
