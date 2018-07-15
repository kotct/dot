(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

(provide 'go)
