(require 'indentation)

;; Don't use SMIE because SMIE sucks.
(setf ruby-use-smie nil)

;; God configurations are in Ruby.
;; TODO Add more Ruby-esque extensions.
(add-to-list 'auto-mode-alist '("\\.god\\'" . ruby-mode))

(provide 'ruby)
