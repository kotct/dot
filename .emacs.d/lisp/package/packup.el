;;; THIS FILE IS NOT LOADED AT STARTUP.
;;; This file is autoloaded on the following symbols:
;;;  kotct/packup-install-dependencies
;;;  kotct/packup-update

(require 'dependencies)
(require 'package)
(require 'cl)

(package-initialize)

(defun kotct/package-up-to-date-p (package)
  "Returns true if PACKAGE is up-to-date.
Does not automatically refresh package list."
  (every (lambda (x) (package-installed-p package (package-desc-version x)))
         (cdr (assq package package-archive-contents))))

(defun kotct/insert-package-row (package-name package-update-date package-desc-version)
  "Inserts a package row into current buffer."
  (let ((inhibit-read-only t))
    (insert (format "[x] %s %s %s\n" package-name package-update-date package-desc-version))))

(defun kotct/packup-initialize-buffer ()
  "Initializes the packup buffer."
  (kill-all-local-variables)
;;  (use-local-map packup-mode-map) ;; mapppppps
  (setq major-mode 'packup-mode
        mode-name "Packup"
        buffer-read-only t ;; read only
        mode-line-buffer-identification "packup")
  (package-refresh-contents)
  (let ((install-list nil))
    (dolist (package kotct/dependency-list)
      (let ()
        (if (or (not (package-installed-p package))
                (not (kotct/package-up-to-date-p package)))
            (apply 'kotct/insert-package-row (cons package (package-desc-version (cadr (assq package package-alist))))))))))

;;;###autoload
(defun kotct/packup ()
  "Creates an interactive buffer to install/update packages."
  (interactive)
  (let ((old-buf (current-buffer))
        (buffer (create-file-buffer "*packup*"))) ;; this is wrong...
    (set-buffer buffer)
    (kotct/packup-initialize-buffer)
    (pop-to-buffer-same-window buffer)))

;;;###autoload
(defun kotct/packup-install-dependencies (no-refresh &optional update)
  "Installs the dependencies.
With a non-nil or prefix arg NO-REFRESH, do not refresh package list.
If UPDATE is non-nil, out-of-date packages will be updated."
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
               (if (y-or-n-p "Auto install/update these package?")
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
