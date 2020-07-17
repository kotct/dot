(eval-when-compile (require 'cc-mode))

(require 'indentation)

;; set style to linux
(setf c-default-style "linux")

;; use tab-width for indentation
(kotct/setf-tab c-basic-offset)

;; use smart tabs
(smart-tabs-insinuate 'c)

(provide 'c)
