;;; C-x C-c: close current frame or, with confirmation, emacs session
;;; C-c C-x C-c: old C-x C-c

(defun kotct/close-current-frame-or-emacs (&optional arg)
  "Closes the current frame or Emacs.

Kills Emacs if 1 frame is in (frame-list) and not a server process,
or if passed an argument, but just current frame if otherwise."
  (interactive "P")

  (condition-case nil
      (let ((frame-list (frame-list)))
        (if (or (= (length frame-list) 1) arg)
            (if (y-or-n-p "Kill Emacs?")
                (save-buffers-kill-emacs)
              (if (and (boundp 'server-process)
                       (y-or-n-p "Delete current frame?"))
                  (delete-frame)
                (message "Didn't do anything, promise!")))
          (delete-frame)))
    (error (kill-emacs))))

;; bind old C-x C-c to C-c C-x C-c
(global-set-key (kbd "C-c C-x C-c") (key-binding (kbd "C-x C-c")))
(global-set-key (kbd "C-x C-c") #'kotct/close-current-frame-or-emacs)

;; Set yes-or-no prompt to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;; i recommend some hydrocodone
(provide 'pane-management)
