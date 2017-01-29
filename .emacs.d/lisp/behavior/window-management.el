
;;; windmove
(global-set-key (kbd "C-s-l") #'windmove-up)
(global-set-key (kbd "C-s-k") #'windmove-down)
(global-set-key (kbd "C-s-j") #'windmove-left)
(global-set-key (kbd "C-s-;") #'windmove-right)

;;; buffer-move
(global-set-key (kbd "C-M-s-l") #'buf-move-up)
(global-set-key (kbd "C-M-s-k") #'buf-move-down)
(global-set-key (kbd "C-M-s-j") #'buf-move-left)
(global-set-key (kbd "C-M-s-;") #'buf-move-right)

(provide 'window-management)
