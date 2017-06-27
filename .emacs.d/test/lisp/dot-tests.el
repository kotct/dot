(require 'buttercup)

(defun kotct/load-corresponding ()
  (when noninteractive
    (assert (string-match-p "-test\\.el$" load-file-name))
    (load-file
     (replace-regexp-in-string
      "-test\\.el$" ".el"
      (replace-regexp-in-string
       (regexp-quote "/test/") "/"
       load-file-name)))))

(defun kotct/run-tests ()
  (setf buttercup-suites nil)
  (load "~/.emacs.d/test/lisp/behavior/completion-c-test.el" nil t)
  (let ((buttercup-reporter
         (lambda (event arg)
           (condition-case err
               (buttercup-reporter-adaptive event arg)
             (error
              ;; avoid error on test failure
              (unless (and (not noninteractive)
                           (equal (second err) ""))
                (error (second err))))))))
    (buttercup-run)))

(provide 'dot-tests)
