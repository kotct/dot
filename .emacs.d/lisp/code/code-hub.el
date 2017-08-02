(kotct/hub "code"
           (editorconfig-c
            magit-c
            smartparens-c
            indentation
            code-navigation))

;;; load individual language files via language-hub
(add-to-list 'load-path (concat (file-name-directory load-file-name) "languages/"))
(require 'language-hub)

(provide 'code-hub)
