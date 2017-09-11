(require 'buttercup)

(eval-when-compile (require 'cl))

(defun kotct/quietly-load-file (filename &optional throw-error)
  "Load FILENAME, but do not send a message on success.
If loading throws an error, send a message with FILENAME and the error.
If THROW-ERROR is non-nil, re-throw the caught error."
  (condition-case err
        (load filename nil 'nomessage)
      (error
       (message (format "Error while loading %s:\n%s" file (error-message-string err)))
       (if throw-error
           (error (cdr err))))))

;; test helper functions
(defun kotct/load-corresponding ()
  "Load the file corresponding to the current test.
Only works when called in script mode."
  (when noninteractive
    (assert (string-match-p "-test\\.el$" load-file-name))
    (kotct/quietly-load-file
     (replace-regexp-in-string
      "-test\\.el$" ".el"
      (replace-regexp-in-string
       (regexp-quote "/test/") "/"
       load-file-name)))))

(defmacro kotct/it-binds (fun binding)
  "Generate a spec in the current test that makes sure FUN
is bound to BINDING.

For example, if `magit-status' is bound to \\[magit-status], then to use,
write in the test: (kotct/it-binds magit-status \"\\[magit-status]\")"
  `(it ,(format "binds %s to %s" fun binding)
     (expect (global-key-binding (kbd ,binding)) :to-be #',fun)))

(defun kotct/run-tests ()
  "Run all emacs lisp tests for the kotct/dot emacs configuration."
  (interactive)
  (setf buttercup-suites nil)
  (dolist (file (directory-files-recursively
                 (file-name-directory (symbol-file #'kotct/run-tests))
                 "-test\\.el\\'"))
    (kotct/quietly-load-file file))
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
