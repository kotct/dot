(require 'package)

;; Add MELPA Stable
(add-to-list 'package-archives
             '("melpa-stable" .
               "https://stable.melpa.org/packages/"))

;; Add GNU ELPA repository
(add-to-list 'package-archives
             '("gnu" .
               "https://elpa.gnu.org/packages/"))

;; Add MELPA repository
(add-to-list 'package-archives
             '("melpa" .
               "https://melpa.org/packages/"))

(if (version< emacs-version "25.1")
    (defvar kotct/package-ordered-archives
      '("melpa-stable"
        "gnu"
        "melpa")
      "Define the preferred order of package repositories.
If a repository is earlier in the list, it is preferred over
a repository that is later in the list.

This variable is succeeded by `package-archive-priorities'
in emacs 25 and later.")
  (setf package-archive-priorities
        '(("melpa-stable" . 20)
          ("gnu" . 10)
          ("melpa" . 0))))

;; (require 'packup)

(provide 'repositories)
