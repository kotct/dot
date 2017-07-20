(load-file "~/.emacs.d/lisp/package/dependencies.el")
(load-file "~/.emacs.d/lisp/package/repositories.el")

(package-initialize)
(package-refresh-contents)

(mapc #'package-install kotct/dependency-list)
