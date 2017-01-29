(require 'package)

;; Add Marmalade repository
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))

;; Add ELPA repository
(add-to-list 'package-archives
             '("elpa" .
               "http://tromey.com/elpa/"))

;; Add MELPA repository
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/"))

;; Add GNU repository
(add-to-list 'package-archives
             '("gnu" .
               "http://elpa.gnu.org/packages/"))

(require 'packup)

(provide 'repositories)
