(defvar languages
  '(c
    elisp
    elixir
    fish
    ruby
    rust
    java
    sh
    web-c
    go)
  "A list of packages configured by the language-hub.")

(let ((to-compile ()))
  (dolist (feature languages)
    (setf to-compile
          (cons (concat (file-name-directory load-file-name) (symbol-name feature) ".el") to-compile))
    (require feature))
  (setf kotct/files-to-compile
        (append kotct/files-to-compile to-compile)))

(provide 'language-hub)
