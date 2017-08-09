(load-file "~/.emacs.d/lisp/package/dependencies.el")
(load-file "~/.emacs.d/lisp/package/repositories.el")

(package-initialize)
(package-refresh-contents)

;; add undercover for coverage reporting
(setf kotct/dependency-list (cons 'undercover kotct/dependency-list))

(mapc #'package-install kotct/dependency-list)
