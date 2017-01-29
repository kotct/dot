;;; smex
(smex-initialize)
(global-set-key (kbd "M-x") #'smex)
;; keep things contained within ~/.emacs.d
(setf smex-save-file "~/.emacs.d/smex-items")

(provide 'smex-c)
