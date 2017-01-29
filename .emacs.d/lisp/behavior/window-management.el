;;; C-s-l: switch focus to window above current
;;; C-s-k: switch focus to window below current
;;; C-s-j: switch focus to window to the left of current
;;; C-s-;: switch focus to window to the right of current
;;; C-M-s-l: move current buffer to window above current
;;; C-M-s-k: move current buffer to window below current
;;; C-M-s-j: move current buffer to window to the left of current
;;; C-M-s-;: move current buffer to window to the right of current

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
