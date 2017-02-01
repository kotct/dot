;;; C-c SPC: immediately jump to any visible character
;;;   note: not actually bound in this file
;;; M-z: like C-c SPC but deletes all characters between and including point and the destination char

;; save zapped chars to kill ring
(setf ajz/zap-function #'kill-region)
(global-set-key (kbd "M-z") #'ace-jump-zap-to-char)

(provide 'ace-jump)
