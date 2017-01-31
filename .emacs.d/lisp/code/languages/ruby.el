(setq-default ruby-indent-line global-tab-width)

(smart-tabs-advice ruby-indent-line ruby-indent-level)
(smart-tabs-insinuate 'ruby)

(setf ruby-indent-tabs-mode t)
(setf ruby-use-smie nil)

(add-to-list 'auto-mode-alist '("\\.god\\'" . ruby-mode))

(provide 'ruby)
