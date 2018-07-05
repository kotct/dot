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

(setf package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))

;; (require 'packup)

(provide 'repositories)
