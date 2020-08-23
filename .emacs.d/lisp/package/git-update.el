;;; THIS FILE IS NOT LOADED AT STARTUP.
;;; This file is autoloaded on the following symbols:
;;;  kotct/update-dot-config
;;;  kotct/update-user-config

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

(defvar kotct/directory "~/.emacs.d"
  "The path to your instance of kotct/dot or one of it's subdirectories.")

(defun kotct/git-repository-diff (directory)
  "Fetches in DIRECTORY and returns the difference between the common ancestor of HEAD and origin/master in DIRECTORY."
  (let ((default-directory directory))
    (kotct/run-git "fetch" "origin" "master")
    (kotct/run-git "diff" "HEAD...origin/master")))

(defun kotct/git-repository-is-behind (directory)
  "Fetch DIRECTORY's origin/master and diff to see if there are any new remote commits."
  (not (string= (kotct/git-repository-diff directory) "")))

(defun kotct/git-current-branch (directory)
  (let ((default-directory directory))
    (kotct/run-git "branch" "--show-current")))

(defun kotct/git-branch-is-master (directory)
  (string= (string-trim (kotct/git-current-branch directory)) "master"))

(defun kotct/user-fetch-config (username)
  "Fetch USERNAME's personal config from GitHub, out of the repository USERNAME/.emacs."
  (message "fetching config for %s" username)
  (let ((default-directory "~/.emacs.d/lisp/user/users/")
        (url (format "https://github.com/%s/.emacs.git" username)))
    (kotct/run-git "clone" url username)))

(defun kotct/update-git-repository (directory &optional auto-update)
  "Fetches changes in DIRECTORY, and pulls from DIRECTORY if instructed by the user or AUTO-UPDATE is true.
Returns true if an update occured."
  (if (kotct/git-branch-is-master directory)
      (if (kotct/git-repository-is-behind directory)
          ;; create new buffer and save old buffer
          (let ((old-buffer (current-buffer))
                (buffer (get-buffer-create (format "*Diff %s*" directory))))
            (set-buffer buffer)
            (let ((inhibit-read-only t))
              ;; erase, fill, and show new buffer
              (erase-buffer)
              (insert (format "%s" (kotct/git-repository-diff directory)))
              (pop-to-buffer-same-window buffer)
              ;; prompt user about updating and update if they want
              (let ((updating (or auto-update
                                  (y-or-n-p (format "Pull the most recent changes to %s?" directory)))))

                (if updating
                    (let ((default-directory directory))
                      (kotct/run-git "pull" "--rebase" "origin" "master")
                      (message (format "%s updated!" directory)))
                  (message "Okay we promise we didn't do anything!"))

                ;; reset buffer what not
                (kill-buffer buffer)
                (set-buffer old-buffer)
                updating)))
        (message (format "%s is up to date." directory))
        nil)
    (error (format "Error: kotct/update-git-repository doesn't support updating non-master branches, and %s is on branch %s."
                   directory
                   (string-trim (kotct/git-current-branch directory))))
    nil))

;;;###autoload
(defun kotct/update-dot-config (&optional auto-update)
  "Update the doct config git repository if the dot config git repository is out of date.
Will auto update if AUTO-UPDATE is true."
  (interactive "P")
  (kotct/update-git-repository kotct/directory auto-update))

;;;###autoload
(defun kotct/update-user-config (&optional auto-update username)
  "Update USERNAME's personal config git repository if the USERNAME's personal config is out of date.
If USERNAME is nil, prompt for a username.
Will auto update if AUTO-UPDATE is true."
  (interactive "P")
  (unless username
    (setf username
          (kotct/user-ask-username "Update config for: " 'require-match)))
  (if (kotct/update-git-repository (format "~/.emacs.d/lisp/user/users/%s" username) auto-update)
      (if (eq username kotct/user-current-username)
          ;; reload the config
          (kotct/user-switch-username username)
        (message (format "%s's config has been udpated!" username)))
    (message (format "%s's config is up to date!" username))))

(provide 'git-update)
