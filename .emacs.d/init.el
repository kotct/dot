;;; kotct.emacs

;;; autoload configuration

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setf generated-autoload-file "~/.emacs.d/lisp/kotct-loaddefs.el")


;;; hub initialization
(defvar kotct/hub-list
  '("package"
    "file"
    "visual")
  "A list of hubs to load at init.")

;; populated by kotct/hub
(defvar kotct/files-to-compile
  nil
  "A list of files that ought to be byte-compiled.")

;; used to define hubs in hub files
(defmacro kotct/hub (hubname features &optional autoloads)
  (let ((feature-var (intern (concat "kotct/" hubname "-features"))))
    `(progn (defvar ,feature-var
              ',features
              ,(concat "A list of features to require from within the " hubname " hub."))

            ,(if autoloads '(update-directory-autoloads (file-name-directory load-file-name)))

            (setf kotct/files-to-compile
                  (append kotct/files-to-compile
                          (mapcar (lambda (x)
                                    (concat (file-name-directory load-file-name) (symbol-name x) ".el"))
                                  (append ',features ',autoloads))))

            (mapc #'require ,feature-var))))

;; add hub directories to load path
(let ((default-directory "~/.emacs.d/lisp/"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-to-load-path kotct/hub-list))

;; if byte-compiled files are out of date, load newer version
(let ((load-prefer-newer t))
  ;; require all hubs
  (mapc (lambda (hub)
		  (require (intern (concat hub "-hub"))))
		kotct/hub-list))

;; load autoloads
(require 'kotct-loaddefs)


;;; byte compilation
;; TODO
(let* ((to-eval `(let ((default-directory "~/.emacs.d/lisp/"))
                   (add-to-list 'load-path default-directory)
                   (normal-top-level-add-to-load-path ',kotct/hub-list)
                   (batch-byte-compile t)))
      (args `("config-compilation" "*config-compilation*" "emacs" "--batch" "--eval" ,(format "%S" to-eval) ,@kotct/files-to-compile)))
  (message "%s" args)
  (apply #'start-process args))


(if (list-load-path-shadows)
	(message "There are shadowed files on your load path.
This could indiciate an issue with your emacs installation.
Despite this, your config appears to have loaded successfully.")
  (message "Your config appears to have loaded successfully."))
