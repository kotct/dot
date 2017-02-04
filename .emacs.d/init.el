;;; dot/.emacs

;; Initialize the package repository.
(package-initialize)

;;; Autoload Configuration
(setf generated-autoload-file "~/.emacs.d/lisp/kotct-loaddefs.el")


;;; Hub Initialization
(defvar kotct/hub-list
  '("package"
    "file"
    "visual"
    "behavior"
    "code"
    "user")
  "A list of hubs to load at init.")

;; populated by kotct/hub
(defvar kotct/files-to-compile
  nil
  "A list of files that ought to be byte-compiled.")

;; used to define hubs in hub files
(defmacro kotct/hub (hubname features &optional autoloads)
  "Loads the hub denoted by HUBNAME.

Defines a variable of the format kotct/HUBNAME-features to be the
list FEATURES.

Adds a list containing all of the files that need to be compiled
in that directory to `kotct/files-to-compile'.

If AUTOLOADS is non-nil, update the autoloads for that directory."
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


;;; Asynchronous Byte Compilation
(let* ((to-eval `(let ((default-directory "~/.emacs.d/lisp/"))
                   (package-initialize)
                   (add-to-list 'load-path default-directory)
                   (normal-top-level-add-to-load-path ',kotct/hub-list)
                   (batch-byte-compile t)))
       ;; command-line args as a list
       (args `("config-compilation" "*config-compilation*" "emacs" "--batch" "--eval" ,(format "%S" to-eval) ,@kotct/files-to-compile)))
  ;; start the process in *config-compilation* buffer
  (apply #'start-process args))

;; Warn the user about shadowed files on the load path.  This usually
;; happens when one keeps .elc files from removed .el files.
(if (list-load-path-shadows)
    (message "There are shadowed files on your load path.
This could indicate an issue with your emacs installation.
Despite this, your config appears to have loaded successfully.")
  (message "Your config appears to have loaded successfully. Rock on!"))

;; Kill the buffer corresponding to `generated-autoload-file'.  After
;; loading autoloads, we don't need it anymore.
(let ((loaddefs-buffer (get-buffer (file-name-nondirectory generated-autoload-file))))
  (if loaddefs-buffer
      (kill-buffer loaddefs-buffer)))


;;; Customization File
(setf custom-file "~/.emacs.d/custom.el")
