(require 'package)

;; Add Marmalade repository
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))

;; Add MELPA repository
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/"))

;; Add MELPA Stable
(add-to-list 'package-archives
             '("melpa-stable" .
               "https://stable.melpa.org/packages/"))

;; Add GNU ELPA repository
(add-to-list 'package-archives
             '("gnu" .
               "http://elpa.gnu.org/packages/"))

(setf package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu" . 10)
        ("marmalade" . 10)
        ("melpa" . 0)))

;; (require 'packup)

(provide 'repositories)
