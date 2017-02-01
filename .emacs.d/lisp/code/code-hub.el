(add-to-list 'load-path (concat (file-name-directory load-file-name) "languages/"))

(kotct/hub "code"
           (editorconfig-c
            magit-c
            indentation
            code-navigation
            language-hub))

(provide 'code-hub)
