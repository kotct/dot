;;; THIS FILE IS NOT LOADED AT STARTUP.
;;; This file is autoloaded on the following symbols:
;;;  kotct/packup
;;;  kotct/packup-install-dependencies
;;;  kotct/packup-update

(require 'dependencies)
(require 'package)
(require 'cl)

(package-initialize)

(defvar kotct/packup-marker-char ?x
  "In Packup, the current mark character.
This is what the do-commands look for, and the flag the mark-commands store.")

(defun kotct/package-up-to-date-p (package)
  "Returns true if PACKAGE is up-to-date.
Does not automatically refresh package list."
  (every (lambda (x) (package-installed-p package (package-desc-version x)))
         (cdr (assq package package-archive-contents))))

(defun kotct/packup-insert-package-row (package-name package-desc-version)
  "Inserts a package row into current buffer."
  (let ((inhibit-read-only t))
    (insert (format "[%c] %s -> %s\n" kotct/packup-marker-char package-name package-desc-version))))

(defun kotct/packup-mark-packages-in-region (start end)
  (let ((inhibit-read-only t))
    (if (> start end)
        (error "start > end"))
    (goto-char start)			; assumed at beginning of line
    (while (< (point) end)
      (forward-char 1)
      (progn
        (delete-char 1)
        (insert kotct/packup-marker-char))
      (forward-line 1))))

(defun kotct/packup-repeat-over-lines (arg function)
  "Helper function for iterating over lines ARG times, and applying FUNCTION on each line."
  (while (and (> arg 0) (not (eobp)))
    (setq arg (1- arg))
    (beginning-of-line)
    (save-excursion (funcall function))
    (forward-line)))

(defun kotct/packup-marker-regexp ()
  (concat "^\\[" (regexp-quote (char-to-string kotct/packup-marker-char)) "\\]"))

(defmacro kotct/packup-map-over-marks (body)
  "Call BODY on each marked line in current buffer."
  `(prog1
       (let ((inhibit-read-only t) case-fold-search found results)
         (let ((regexp (kotct/packup-marker-regexp)) next-position)
           (save-excursion
             (goto-char (point-min))
             ;; remember position of next marked package before BODY
             ;; can insert lines before the just found package,
             ;; confusing us by finding the same marked package again
             ;; and again and...
             (setq next-position (and (re-search-forward regexp nil t)
                                      (point-marker))
                   found (not (null next-position)))
             (while next-position
               (goto-char next-position)
               (setq results (cons ,body results))
               ;; move after last match
               (goto-char next-position)
               (forward-line 1)
               (set-marker next-position nil)
               (setq next-position (and (re-search-forward regexp nil t)
                                        (point-marker))))))
         (if t
             results
           nil))))

(defun kotct/packup-get-package-name ()
    "Gets package name on current line.
Returns \"\" if there is no package name on the line."
    (save-excursion
      (beginning-of-line)
      (forward-char 4)
      (let ((point (point-marker)))
        (if (re-search-forward " " nil t)
            (prog2
                (backward-char)
                (buffer-substring point (point-marker)))
          ""))))

(defun kotct/packup-do-update ()
  "Executes update in current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let ((kotct/dependency-list (kotct/packup-map-over-marks (intern (kotct/packup-get-package-name)))))
        (kotct/packup-install-dependencies nil t t))))
  (kotct/packup-refresh))

(defun kotct/packup-mark (arg &optional interactive)
  "Mark the package for update at point in the Packup buffer.
If a region is selected, mark all the packages in the region.
If an prefix-arg is passed mark ARG times."
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
         (insert kotct/packup-marker-char)))))))

(defun kotct/packup-unmark (arg &optional interactive)
  "Unmark the package for update at point in the Packup buffer.
If a region is selected, unmark all the packages in the region.
If an prefix-arg is passed unmark ARG times."
  (interactive (list current-prefix-arg))
  (let ((kotct/packup-marker-char ?\040))
    (kotct/packup-mark arg interactive)))

(defun kotct/packup-mark-all ()
  (interactive)
  (save-excursion
    (kotct/packup-mark-packages-in-region (point-min) (point-max))))


(defun kotct/packup-unmark-all ()
    (interactive)
    (save-excursion
      (let ((kotct/packup-marker-char ?\040))
        (kotct/packup-mark-packages-in-region (point-min) (point-max)))))

(defun kotct/packup-initialize-buffer-contents ()
  (let ((inhibit-read-only t))
    (kill-region (point-min) (point-max)))
  (package-refresh-contents)
  (let ((install-list nil))
    (dolist (package kotct/dependency-list)
      (if (or (not (package-installed-p package))
              (not (kotct/package-up-to-date-p package)))
          (apply 'kotct/packup-insert-package-row (list package (package-desc-version (cadr (assq package package-alist)))))))))

(defun kotct/packup-initialize-buffer ()
  "Initializes the packup buffer."
  (kill-all-local-variables)
  (use-local-map packup-mode-map)
  (setq major-mode 'packup-mode
        mode-name "Packup"
        buffer-read-only t)
  (kotct/packup-initialize-buffer-contents))

(defun kotct/packup-refresh ()
  "Refreshes packages in current buffer."
  (interactive)
  (if (eq major-mode 'packup-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (kotct/packup-initialize-buffer-contents))))

(defun kotct/packup-help ()
  (interactive)
  (message "g-refresh m-mark u-unmark x-execute ?-help"))

(defvar packup-mode-map
  (let ((map (make-keymap)))
    (define-key map "m" 'kotct/packup-mark)
    (define-key map "M" 'kotct/packup-mark-all)
    (define-key map "u" 'kotct/packup-unmark)
    (define-key map "U" 'kotct/packup-unmark-all)
    (define-key map "x" 'kotct/packup-do-update)
    (define-key map "g" 'kotct/packup-refresh)
    (define-key map "?" 'kotct/packup-help)
    map))

;;;###autoload
(defun kotct/packup ()
  "Creates an interactive buffer to install/update packages."
  (interactive)
  (let ((old-buf (current-buffer))
        (buffer (get-buffer-create "*packup*")))
    (set-buffer buffer)
    (kotct/packup-initialize-buffer)
    (if (= (point-min) (point-max))
        (progn
         (kill-buffer buffer)
         (message "Nothing to update!"))
      (pop-to-buffer-same-window buffer))))

;;;###autoload
(defun kotct/packup-install-dependencies (no-refresh &optional update auto-update)
  "Installs the dependencies.
With a non-nil or prefix arg NO-REFRESH, do not refresh package list.
If UPDATE is non-nil, out-of-date packages will be added to install list.
If AUTO-UPDATE is non-nil, out-of-date/uninstalled packages will be updated."
  (interactive "P")

  (unless no-refresh (package-refresh-contents))

  (let ((install-list nil))
    (dolist (package kotct/dependency-list)

      (let ((updating nil))
        (if (or (not (package-installed-p package))
                (and update (not (kotct/package-up-to-date-p package)) (setf updating t)))
            (add-to-list 'install-list
                         (cons package
                               ;; haxily say we need the next version by adding a .1 to the version
                               ;; ie if we have version 2.3.0 ask for 2.3.0.1
                               (if updating
                                   (list (append (package-desc-version (cadr (assq package package-alist)))
                                                 '(1)))))))))

    (if install-list

        (progn (with-output-to-temp-buffer "*packup: packages to upgrade*"
                 (princ "Packages to be installed:")
                 (dolist (package install-list)
                   (message (symbol-name (car package)))
                   (terpri)
                   (princ (symbol-name (car package)))
                   (princ (if (cdr package) " (update)" " (install)"))))
               (if (or auto-update
                       (y-or-n-p "Auto install/update these package(s)?"))
                   (progn (package-download-transaction (package-compute-transaction () install-list))
                          (kill-buffer "*packup: packages to upgrade*")
                          (message "Dependency installation completed."))
                 (let ((manual-install-list nil))
                   (dolist (package install-list)
                     (if (y-or-n-p (format "Package %s is %s. Install it? "
                                           (car package)
                                           (if (cdr package) "out of date" "missing")))
                         (add-to-list 'manual-install-list package)))
                   (progn (package-download-transaction (package-compute-transaction () manual-install-list))
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
