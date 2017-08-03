;; NOTE: this file is designed to be exclusively run in script mode

(package-initialize)

(load-file (concat (file-name-directory load-file-name) "/dot-tests.el"))

(when (require 'undercover nil 'noerror)
  ;; undercover can't search for matching files recursively
  ;; override a local function that fixes this
  (advice-add
   #'undercover--wildcards-to-files :override
   (lambda (_wildcards)
     (mapcar (lambda (filename)
               ;; undercover adds a leading "/", so remove the
               ;; "/" at the beginning of the absolute filename
               (substring (file-truename filename) 1))
             (directory-files-recursively "~/.emacs.d/lisp/" ".el$"))))
  (undercover))

(kotct/run-tests)
