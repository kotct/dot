(setf global-tab-width 4)
(setq-default tab-width global-tab-width)
(setf smie-indent-basic global-tab-width)

(setq-default indent-tabs-mode nil)
(setf backward-delete-char-untabify-method nil)

(provide 'indentation)
