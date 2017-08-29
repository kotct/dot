(require 'indentation)

;; Synchronize `sh-basic-offset' and `sh-indentation' to the global tab size
(kotct/setf-tab sh-basic-offset)
(kotct/setf-tab sh-indentation)

;; Always use indent-tabs-mode in sh-mode
(add-hook 'sh-mode-hook (lambda () (setf indent-tabs-mode t)))

;; Indent case labels at the same level as the case statement
(setf sh-indent-for-case-label 0)

;; Indent case alts to +
(setf sh-indent-for-case-alt '+)

(provide 'sh)
