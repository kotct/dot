(with-eval-after-load 'magit
  (let ((popups-to-add-sign '(magit-merge-popup
                              magit-rebase-popup)))
    (mapcar (lambda (popup)
              (magit-define-popup-option
                popup
                ?S "Sign using gpg" "--gpg-sign=" #'magit-read-gpg-secret-key))
            popups-to-add-sign)))

(global-set-key (kbd "C-x m") #'magit-status)

(provide 'magit-c)
