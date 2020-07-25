;; NOTE: this file is designed to be exclusively run in script mode

(package-initialize)

(load-file (concat (file-name-directory load-file-name) "/dot-tests.el"))

(defun parent-dir (str)
  (file-name-directory (directory-file-name str)))

(let ((coverage-report-file
       (concat
        (parent-dir (parent-dir (parent-dir (parent-dir load-file-name)))) "coverage-final.json")))
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
    (message (format "Running undercover, writing report to %s" coverage-report-file))
    (undercover (:report-file coverage-report-file)
                (:send-report nil))))

(kotct/run-tests)
