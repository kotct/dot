;; NOTE: this file is designed to be exclusively run in script mode

(package-initialize)

(unless (package-installed-p 'undercover)
  (require 'repositories "~/.emacs.d/lisp/package/repositories")
  (package-refresh-contents)
  (package-install 'undercover 'dont-select))

(load-file (concat (file-name-directory load-file-name) "/dot-tests.el"))

(require 'undercover)
;; undercover can't search for matching files recursively
;; override a local function that fixes this
(advice-add
 #'undercover--wildcards-to-files :override
 (lambda (wildcards)
   (mapcar (lambda (filename)
             ;; undercover adds a leading "/", so remove the
             ;; "/" at the beginning of the absolute filename
             (substring (file-truename filename) 1))
           (directory-files-recursively "~/.emacs.d/lisp/" ".el$"))))
(undercover)

(kotct/run-tests)
