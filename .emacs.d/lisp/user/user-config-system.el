;;; C-x C-z: switch personal configs

(require 'git)

(defvar kotct/user-current-username
  nil
  "The username associated with the currently loaded personal config.")

(defvar kotct/user-default-username-file
  "~/.emacs.d/lisp/user/default-username"
  "The file that sets the default username for the machine.  (Ignored by git.)")

(defmacro kotct/personal-packages (&rest packages)
  (setf kotct/dependency-list (append kotct/dependency-list packages))
  (kotct/check-dependency-list))

(defun kotct/run-git (&rest args)
  "Run a git command, specified by ARGS."
  (let ((process-environment (cons "GIT_TERMINAL_PROMPT=0" process-environment)))
    (with-temp-buffer
      (let ((exit-code (apply 'call-process
                              (append (list (executable-find "git")
                                            nil (current-buffer) nil
                                            "--no-pager" )
                                      args))))
        (if (zerop exit-code)
            (buffer-string)
          (error "Error running git %s\n%s" args (buffer-string)))))))

(defun kotct/user-fetch-config (username)
  "Fetch USERNAME's personal config from GitHub, out of the
repository USERNAME/.emacs."
  (message "fetching config for %s" username)
  (let ((default-directory "~/.emacs.d/lisp/user/users")
        (url (format "https://github.com/%s/.emacs.git" username)))
    (kotct/run-git "clone" url username)))

(defun kotct/user-update-config (&optional username)
  "Update USERNAME's personal config git repository.
If USERNAME is nil, the value of `kotct/user-current-username' is used."
  (interactive)
  (unless username (setf username kotct/user-current-username))
  (let ((default-directory (format "~/.emacs.d/lisp/user/users/%s" username)))
    (kotct/run-git "pull" "origin" "master"))
  (if (eq username kotct/user-current-username)
      ;; reload the config
      (kotct/user-switch-username username))
  (message "@%s's config updated!" username))

(defun kotct/user-get-default-username ()
  "Look up the default username set on this machine."
  (let ((filename kotct/user-default-username-file))
    (if (file-exists-p filename)
        (with-temp-buffer
          (insert-file-contents filename)
          (replace-regexp-in-string "\n\\'" "" (buffer-string)))
      (if (y-or-n-p "Would you like to set a default personal config?")
          (kotct/user-write-default-username (kotct/user-ask-username "Choose default username: "))
        ;; default to base-config, save so that we don't keep asking
        (kotct/user-write-default-username "base-config")))))

(defun kotct/user-set-default-username (&optional username)
  "Set the default username to USERNAME, and switch to USERNAME's personal config.
If USERNAME is nil, prompt the user for the username."
  (interactive)
  (unless username (setf username (kotct/user-ask-username "Choose default username: ")))
  (kotct/user-write-default-username username)
  (kotct/user-switch-username username))

(defun kotct/user-write-default-username (username)
  "Write the default username as USERNAME to `kotct/user-default-username-file'. "
  (with-temp-file kotct/user-default-username-file
    (erase-buffer)
    (insert username))
  username)

(defun kotct/user-load-username (username)
  "Load the personal config of USERNAME.
Fetch the personal config from GitHub if it doesn't exist locally."
  (let ((personal-dir (concat "~/.emacs.d/lisp/user/users/" username)))
    (unless (file-exists-p personal-dir)
      (if (y-or-n-p (format "%s's config has not been fetched from GitHub. Fetch it now?" username))
          (kotct/user-fetch-config username)
        (error "No config for %s." username)))
    (add-to-list 'load-path personal-dir)
    (message "Loading @%s's configuration..." username)
    (require (intern (concat username "-hub")))
    (message "Loaded successfully!")
    (setf kotct/user-current-username username)))

(defun kotct/user-ask-username (prompt)
  "Use ido to ask the user for a username, prompting the user with PROMPT."
  (ido-completing-read prompt
                       (directory-files "~/.emacs.d/lisp/user/users/" nil "^[^.].*$")))

(defun kotct/user-unload-username (&optional username)
  "Unload the personal config of USERNAME, or if USERNAME is nil, `kotct/user-current-username'."
  (unless username (setf username kotct/user-current-username))
  (unload-feature (intern (concat username "-hub")) 'force))

(defun kotct/user-switch-username (&optional username)
  "Unload the current personal config and load USERNAME's personal config.
If USERNAME is nil, prompt for a username."
  (interactive)
  (message "switch: %s" username)
  (unless username
    (setf username
          (kotct/user-ask-username "Switch to username: ")))
  (kotct/user-unload-username)
  ;; make sure that if the load fails we reload the stuff we just unloaded
  ;; so that we are actually loaded with current-username's config

  (message "Loading for username: %s" username)
  (condition-case err
      (kotct/user-load-username username)
    (test (message "Handling error, username: %s" kotct/user-current-username)
           (kotct/user-load-username kotct/user-current-username)
           (signal (car err) (cdr err))))
  ;; maybe then also reload/rerun all the hooks and stuff for open buffers
  )

(global-set-key (kbd "C-x C-z") #'kotct/user-switch-username)

(kotct/user-load-username (kotct/user-get-default-username))

(provide 'user-config-system)
