(require 'package)

;; Add MELPA Stable
(add-to-list 'package-archives
             '("melpa-stable" .
               "https://stable.melpa.org/packages/"))

;; Add GNU ELPA repository
(add-to-list 'package-archives
             '("gnu" .
               "http://elpa.gnu.org/packages/"))

;; Add MELPA repository
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/"))

(setf package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))

;; (require 'packup)

(provide 'repositories)
