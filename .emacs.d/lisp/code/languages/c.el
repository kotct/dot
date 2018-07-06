(require 'indentation)

;; set style to linux
(setf c-default-style "linux")

;; use tab-width for indentation
(kotct/setf-tab c-basic-offset)

;; use smart tabs
(smart-tabs-insinuate 'c)

(add-hook 'c-mode-hook
          (lambda ()
            (auto-complete-mode -1)
            (company-mode +1)
            (irony-mode +1)
            (add-to-list 'company-backends 'company-irony)))

(add-hook 'irony-mode-hook
          'irony-cdb-autosetup-compile-options)

(provide 'c)
