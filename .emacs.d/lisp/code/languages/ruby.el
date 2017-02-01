;; Set ruby-indent-level to global-tab-width
(setq-default ruby-indent-level global-tab-width)

;; ;; Provide advice to Smart Tabs about how to handle Ruby's special
;; ;; indentation variables.
;; (smart-tabs-advice ruby-indent-line ruby-indent-level)

;; Add Smart Tabs language support for Ruby using the
;; `smart-tabs-add-language-support' macro.
(smart-tabs-add-language-support ruby ruby-mode-hook
  ((ruby-indent-line . ruby-indent-level)
   (ruby-indent-level . ruby-indent-level)))

;; Apply Ruby language support to Smart Tabs.
(smart-tabs-insinuate 'ruby)

;; Use tabs mode in Ruby by default.
(setf ruby-indent-tabs-mode t)

;; Don't use SMIE because SMIE sucks.
(setf ruby-use-smie nil)

;; God configurations are in Ruby.
;; TODO Add more Ruby-esque extensions.
(add-to-list 'auto-mode-alist '("\\.god\\'" . ruby-mode))

(provide 'ruby)
