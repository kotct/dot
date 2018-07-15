;;; THIS FILE IS NOT LOADED AT STARTUP.
;;; This file is autoloaded on the following symbols:
;;;  kotct/packup
;;;  kotct/packup-install-dependencies
;;;  kotct/packup-update

(require 'dependencies)
(require 'package)
(require 'cl)

(package-initialize)

(defvar kotct/packup-marker-char ?*
  "In Packup, the current mark character.
This is what the do-commands look for, and the flag the mark-commands store.")

(defmacro kotct/with-stable-package-archive-contents (body)
  "Run BODY, but if we don't have Emacs 25 or later, let-bind
a modified `package-archive-contents' that only includes the package-desc
from the most-preferred repository."
  (if (version< emacs-version "25.1")
      `(let ((package-archive-contents
              (mapcar (lambda (pkg)
                        ;; first check package-pinned-packages
                        (let* ((pinned-archive (cdr (assoc (car pkg) package-pinned-packages)))
                               (desc (when pinned-archive
                                       (car (cl-member pinned-archive (cdr pkg)
                                                       :key #'package-desc-archive
                                                       :test #'string=)))))
                          (dolist (archive kotct/package-ordered-archives)
                            (when (not desc)
                              ;; no match yet, keep looking
                              (setf desc (car (cl-member archive (cdr pkg)
                                                         :key #'package-desc-archive
                                                         :test #'string=)))))
                          (list (car pkg) desc)))
                      package-archive-contents)))
         ,body)
    `(progn ,body)))

(defun kotct/package-latest-available (package)
  "Get the lastest-available package-desc for PACKAGE, preferring repositories
as listed in `package-archive-priorities' or `kotct/package-ordered-archives'.

Does not automatically refresh the package list."
  (if (version< emacs-version "25.1")
      ;; no package-archive-priorities, so use kotct/package-ordered-archives
      (let* ((versions (cdr (assoc package package-archive-contents)))
             ;; check package-pinned-packages first
             (pinned-archive (cdr (assoc package package-pinned-packages)))
             (found-desc (when pinned-archive
                     (car (cl-member pinned-archive versions
                                     :key #'package-desc-archive
                                     :test #'string=)))))
        (dolist (archive kotct/package-ordered-archives)
          (when (not found-desc)
            ;; no match yet, keep looking
            (setf found-desc (car (cl-member archive versions
                                             :key #'package-desc-archive
                                             :test #'string=)))))
        found-desc)
    ;; package-archive-priorities means we only have to check
    ;; the first package-desc, because that should be from the best repo
    (cadr (assoc package package-archive-contents))))

(defun kotct/package-up-to-date-p (package)
  "Return non-nil if PACKAGE is installed and up-to-date.

Does not automatically refresh package list.
Before Emacs 25, we have to manually check in preferred-repository order."
  (let ((latest (kotct/package-latest-available package)))
    (when latest
      (package-installed-p package (package-desc-version latest)))))

(defun kotct/packup-insert-package-row (package-name package-desc-version)
  "Insert a package row into current buffer."
  (let ((inhibit-read-only t))
    (insert (format "[%c] %s -> %s\n" kotct/packup-marker-char package-name package-desc-version))))

(defun kotct/packup-mark-packages-in-region (start end)
  "Mark all packages in the current packup buffer that are between
START and END. START should be at the beginning of a line."
  (let ((inhibit-read-only t))
    (if (> start end)
        (error "start > end"))
    ;; assume we are at beginning of line
    (goto-char start)
    (while (< (point) end)
      (forward-char 1)
      (delete-char 1)
      (insert kotct/packup-marker-char)
      (forward-line 1))))

(defun kotct/packup-repeat-over-lines (arg function)
  "Helper function for iterating over lines ARG times, and applying FUNCTION on each line."
  (while (and (> arg 0) (not (eobp)))
    (setf arg (1- arg))
    (beginning-of-line)
    (save-excursion (funcall function))
    (forward-line)))

(defun kotct/packup-marker-regexp ()
  "Return a regexp to search for `kotct/packup-marker-char' surrounded by quotes,
as in '[x]'."
  (concat "^\\[" (regexp-quote (char-to-string kotct/packup-marker-char)) "\\]"))

(defmacro kotct/packup-map-over-marks (body)
  "Call BODY on each marked line in current buffer."
  `(let ((inhibit-read-only t)
         (regexp (kotct/packup-marker-regexp))
         case-fold-search next-position results)
     (save-excursion
       (goto-char (point-min))
       ;; remember position of next marked package before BODY
       ;; can insert lines before the just found package,
       ;; confusing us by finding the same marked package again
       ;; and again and...
       (setf next-position (and (re-search-forward regexp nil t)
                                (point-marker)))
       (while next-position
         (goto-char next-position)
         (setf results (cons ,body results))
         ;; move after last match
         (goto-char next-position)
         (forward-line 1)
         (set-marker next-position nil)
         (setf next-position (and (re-search-forward regexp nil t)
                                  (point-marker)))))
     results))

(defun kotct/packup-get-package-name ()
  "Get package name on current line.
Return \"\" if there is no package name on the line."
  (save-excursion
    (beginning-of-line)
    (forward-char 4)
    (let ((point (point-marker)))
      (if (re-search-forward " " nil t)
          (progn
            (backward-char)
            (buffer-substring point (point-marker)))
        ""))))

(defun kotct/packup-do-update ()
  "Execute update in current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let ((kotct/dependency-list (kotct/packup-map-over-marks (intern (kotct/packup-get-package-name)))))
        (kotct/packup-install-dependencies nil 'update 'auto-update))))
  (kotct/packup-refresh))

(defun kotct/packup-mark (arg &optional interactive)
  "Mark the package for update at point in the Packup buffer.
If a region is selected, mark all the packages in the region.
If a prefix-arg is passed mark ARG times."
  (interactive (list current-prefix-arg t))
  (cond
   ((use-region-p)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (kotct/packup-mark-packages-in-region
         (progn (goto-char beg) (line-beginning-position))
         (progn (goto-char end) (line-beginning-position))))))
   (t
    (let ((inhibit-read-only t))
      (kotct/packup-repeat-over-lines
       (prefix-numeric-value arg)
       (lambda ()
         (forward-char 1)
         (delete-char 1)
         (insert kotct/packup-marker-char)))
      (forward-char)))))

(defun kotct/packup-unmark (arg &optional interactive)
  "Unmark the package for update at point in the Packup buffer.
If a region is selected, unmark all the packages in the region.
If an prefix-arg is passed unmark ARG times."
  (interactive (list current-prefix-arg))
  (let ((kotct/packup-marker-char ?\040))
    (kotct/packup-mark arg interactive)))

(defun kotct/packup-mark-all ()
  "Mark all packages in the current packup buffer."
  (interactive)
  (save-excursion
    (kotct/packup-mark-packages-in-region (point-min) (point-max))))


(defun kotct/packup-unmark-all ()
  "Unmark all packages in the current packup buffer."
  (interactive)
  (save-excursion
    (let ((kotct/packup-marker-char ?\040))
      (kotct/packup-mark-packages-in-region (point-min) (point-max)))))

(defun kotct/packup-next ()
  "Go to the next package in the current packup buffer."
  (interactive)
  (move-beginning-of-line 2)
  (forward-char))

(defun kotct/packup-prev ()
  "Go to the previous package in the current packup buffer."
  (interactive)
  (move-beginning-of-line 0)
  (forward-char))

(defun kotct/packup-initialize-buffer-contents ()
  "Initialize contents of the current packup buffer.

This refreshes `package-archive-contents' and finds all packages that need
updating or installing. It then uses this information to write the
contents of the buffer."
  (let ((inhibit-read-only t))
    (kill-region (point-min) (point-max)))
  (package-refresh-contents)
  (let ((install-list nil))
    (dolist (package kotct/dependency-list)
      (when (not (kotct/package-up-to-date-p package))
        (apply #'kotct/packup-insert-package-row (list package (package-desc-version (kotct/package-latest-available package))))))))

(defun kotct/packup-refresh ()
  "Refresh packages in current packup buffer."
  (interactive)
  (when (eq major-mode 'packup-mode)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (kotct/packup-initialize-buffer-contents)
    (if (= (point-min) (point-max))
        (let ((inhibit-read-only t))
          (insert "Everything is up to date!\n"))
      (goto-char 2))))

(defun kotct/packup-help ()
  "Show packup help."
  (interactive)
  (message "g-refresh m-mark u-unmark x-execute ?-help"))

(defvar packup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" #'kotct/packup-mark)
    (define-key map "M" #'kotct/packup-mark-all)
    (define-key map "u" #'kotct/packup-unmark)
    (define-key map "U" #'kotct/packup-unmark-all)
    (define-key map "x" #'kotct/packup-do-update)
    (define-key map "n" #'kotct/packup-next)
    (define-key map "p" #'kotct/packup-prev)
    (define-key map "g" #'kotct/packup-refresh)
    (define-key map "?" #'kotct/packup-help)
    (define-key map "q" #'quit-window)
    map)
  "The keymap for packup-mode.")

(defun kotct/packup-initialize-buffer ()
  "Initialize the packup buffer."
  (kill-all-local-variables)
  (use-local-map packup-mode-map)
  (setf major-mode 'packup-mode
        mode-name "Packup"
        buffer-read-only t)
  (kotct/packup-initialize-buffer-contents))


;;;###autoload
(defun kotct/packup ()
  "Create an interactive buffer to install/update packages."
  (interactive)
  (let ((old-buf (current-buffer))
        (buffer (get-buffer-create "*packup*")))
    (set-buffer buffer)
    (kotct/packup-initialize-buffer)
    (if (= (point-min) (point-max))
        (progn
          (kill-buffer buffer)
          (message "Nothing to update!"))
      (goto-char 2)
      (pop-to-buffer-same-window buffer))))

;;;###autoload
(defun kotct/packup-install-dependencies (no-refresh &optional update auto-update)
  "Install the dependencies.
With a non-nil or prefix arg NO-REFRESH, do not refresh package list.

If UPDATE is non-nil, out-of-date packages will be added to install list.
If AUTO-UPDATE is non-nil, out-of-date/uninstalled packages will be updated."
  (interactive "P")

  (unless no-refresh (package-refresh-contents))

  ;; install-list is a list of cons cells
  ;; the car of each is a package-desc, the cdr is the currently installed package-desc
  (let (install-list)
    (dolist (package kotct/dependency-list)

      (if (or (not (package-installed-p package))
              (and update (not (kotct/package-up-to-date-p package))))
          (add-to-list 'install-list
                       (cons (kotct/package-latest-available package)
                             (cadr (assoc package package-alist))))))

    (if install-list

        (progn (with-output-to-temp-buffer "*packup: packages to upgrade*"
                 (princ "Packages to be installed:")
                 (dolist (package-cons install-list)
                   (message (symbol-name (package-desc-name (car package-cons))))
                   (terpri)
                   (princ (symbol-name (package-desc-name (car package-cons))))
                   (princ (if (cdr package-cons)
                              (format " (update %s -> %s)"
                                      (package-version-join (package-desc-version (cdr package-cons)))
                                      (package-version-join (package-desc-version (car package-cons))))
                            (format " (install %s)"
                                    (package-version-join (package-desc-version (car package-cons))))))))
               (if (or auto-update
                       (y-or-n-p "Auto install/update these package(s)?"))
                   (progn (package-download-transaction
                           (kotct/with-stable-package-archive-contents
                            (package-compute-transaction
                             () (mapcar (lambda (package-cons)
                                          (list (package-desc-name (car package-cons))
                                                (package-desc-version (car package-cons))))
                                        install-list))))
                          (kill-buffer "*packup: packages to upgrade*")
                          (message "Dependency installation completed."))
                 (let ((manual-install-list nil))
                   (dolist (package-cons install-list)
                     (if (y-or-n-p (format "Package %s is %s. Install %s? "
                                           (package-desc-name (car package-cons))
                                           (if (cdr package-cons)
                                               (format "out of date (%s)"
                                                       (package-version-join (package-desc-version (cdr package-cons))))
                                             "missing")
                                           (package-version-join (package-desc-version (car package-cons)))))
                         (add-to-list 'manual-install-list package-cons)))
                   (progn (package-download-transaction
                           (kotct/with-stable-package-archive-contents
                            (package-compute-transaction
                             () (mapcar (lambda (package-cons)
                                          (list (package-desc-name (car package-cons))
                                                (package-desc-version (car package-cons))))
                                        manual-install-list))))
                          (kill-buffer "*packup: packages to upgrade*")
                          (message "Dependency installation completed.")))))

      (message "No dependencies needing installation."))))

;;;###autoload
(defun kotct/packup-update (no-refresh)
  "Update all dependecies.
With a non-nil or prefix arg NO-REFRESH, do not refresh package list."
  (interactive "P")
  (kotct/packup-install-dependencies no-refresh t))


(provide 'packup)
