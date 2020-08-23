(load-file "~/.emacs.d/lisp/package/dependencies.el")
(load-file "~/.emacs.d/lisp/package/repositories.el")
(load-file "~/.emacs.d/lisp/package/packup.el")

(package-refresh-contents)

;; add undercover for coverage reporting
(setf kotct/dependency-list (cons 'undercover kotct/dependency-list))

(kotct/with-stable-package-archive-contents
 (mapc #'package-install kotct/dependency-list))
