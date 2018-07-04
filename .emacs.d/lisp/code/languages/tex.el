(add-hook 'tex-mode-hook (lambda ()
                           (visual-line-mode +1)
                           (if (executable-find ispell-program-name)
                               (flyspell-mode +1))))

(provide 'tex)
