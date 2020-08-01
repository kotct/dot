;; NOTE: this file is designed to be exclusively run in script mode

(package-initialize)

(defun expanded-parent-dir (str)
  (expand-file-name (file-name-directory (directory-file-name str))))

(load-file (concat (expanded-parent-dir load-file-name) "/dot-tests.el"))

(defun kotct/locate-dot-el-files
  (directory-files-recursively
    (concat
      (expanded-parent-dir ;; .emacs.d/
        (expanded-parent-dir ;; .emacs.d/test/
          (expanded-parent-dir ;; .emacs.d/test/lisp/
            load-file-name))) ;; .emacs.d/test/lisp/run-tests.el
      "lisp/") ;; .emacs.d/lisp/
    ".el$"))

(when (require 'undercover nil t)
  (undercover
    (kotct/locate-dot-el-files)
    (:report-file "coverage-final.json")
    (:send-report nil)))

(kotct/run-tests)
