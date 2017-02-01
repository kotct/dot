;;; M-n: go the next occurence of the symbol under point
;;; M-p: go the previous occurence of the symbol under point
;;;   note: these keybindings are enabled as a part of `highlight-symbol-nav-mode'
;;; M-': replace all following occurences of the symbol under point

;; enable highlight-symbol mode for all programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (highlight-symbol-mode +1)
                            (highlight-symbol-nav-mode +1)
                            (local-set-key (kbd "M-'") #'highlight-symbol-query-replace)))

(provide 'code-navigation)
