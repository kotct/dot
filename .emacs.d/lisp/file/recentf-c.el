;;; C-x C-r: find recent files and directories using recentf

;; `require' these because we need some of recentf's things now prior
;; to the autoload at the end of the file.
(require 'recentf)
(require 'cl)

;; Keep only the last 200 saved items.
(setf recentf-max-saved-items 200)

;; Set timer to save `recentf-list' every 5 minutes.
(run-at-time (* 5 60) (* 5 60) #'recentf-save-list)

;; recentf completion using `ido-mode'
;;
;; TODO Remove `~/.emacs.d/recentf` from completion list?
(defun kotct/ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (let ((recent-alist (kotct/recentf-show-basenames-modified-directories
                       (recentf-menu-elements recentf-max-saved-items))))
    (find-file (assoc-default
                (ido-completing-read "Find recent file: " (mapcar #'car recent-alist))
                recent-alist))))

(global-set-key (kbd "C-x C-r") #'kotct/ido-recentf-open)

;; Handle directories (incl. remotes) for recentf.
(defun kotct/file-directory-nonremote-p (filename)
  "Return t if FILENAME names an existing directory.

Remote directories are assumed to exist unless there is already
an open connection that we can use to verify existence."
  (if (or (not (file-remote-p filename))
          (file-remote-p filename nil t))
      (file-directory-p filename)
    (string= (substring filename -1) "/")))

(defun kotct/recentf-show-basenames-modified-directories (l &optional no-dir)
  "Filter the list of menu elements L to show filenames or directory names
sans the rest of their path, using `recentf-show-basenames'.
When a filename is duplicated, it is appended a sequence number if
optional argument NO-DIR is non-nil, or its directory otherwise."
  ;; the list file-name-directory-translators has a one-to-one correspondence
  ;; with list-for-recentf. each element in file-name-directory-translators is
  ;; nil if the corresponding recent file is a file, and is the directory name
  ;; (with tailing slash) if the recent file is a directory.
  ;; translators-cons tracks the last cons cell in file-name-directory-translators,
  ;; allowing us to append to the list in constant time and allowing the algorithm
  ;; as a whole to run in linear time
  (let* ((file-name-directory-translators '())
         (translators-cons nil)
         (list-for-recentf
          (mapcar (lambda (item)
                    (let ((translated nil))
                      (prog1
                          (if (kotct/file-directory-nonremote-p (cdr item))
                              (progn
                                (setf translated (cdr item))
                                (cons (car item) (directory-file-name (cdr item))))
                            item)
                        (if translators-cons
                            (setf (cdr translators-cons) (list translated)
                                  translators-cons (cdr translators-cons))
                          (setf file-name-directory-translators (list translated)
                                translators-cons file-name-directory-translators)))))
                  l))
         (list-with-basenames (recentf-show-basenames list-for-recentf no-dir)))
    (mapcar (lambda (item)
              (if (equal item '("" . "/"))
                  ;; weird edge case
                  '("/" . "/")
                (let ((popped (pop file-name-directory-translators)))
                  (if popped
                      (cons (file-name-as-directory (car item)) popped)
                    item))))
            list-with-basenames)))

;; track when directories are opened and closed
(defun kotct/recentf-track-opened-directory ()
  "Insert the name of the directory just opened with dired into the recent list."
  (and dired-directory
       (recentf-add-file dired-directory)))

(defun kotct/recentf-track-closed-directory ()
  "Update the recent list when a buffer is killed.
That is, remove a non kept directory from the recent list."
  (and dired-directory
       (recentf-remove-if-non-kept dired-directory)))

(add-to-list 'recentf-used-hooks '(dired-mode-hook  kotct/recentf-track-opened-directory))
(add-to-list 'recentf-used-hooks '(kill-buffer-hook kotct/recentf-track-closed-directory))

;; Finally, enable `recentf-mode'
(recentf-mode 1)

(provide 'recentf-c)
