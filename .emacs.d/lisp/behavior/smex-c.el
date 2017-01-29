;;; M-x: ido-completed M-x
;;; C-c C-c M-x: normal M-x

;;; smex
(smex-initialize)
(global-set-key (kbd "M-x") #'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; keep things contained within ~/.emacs.d
(setf smex-save-file "~/.emacs.d/smex-items")

(provide 'smex-c)
