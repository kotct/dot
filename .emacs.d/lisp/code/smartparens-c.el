(require 'smartparens-config)

(smartparens-global-mode t)

(defun kotct/open-on-empty-line (&rest _ignored)
  "Make sure the opening brace or bracket is placed on a new indented line.

Intended for use as a pre-handler with smartparens."
  (message "hi!")
  (unless (string-match "\\s-" (buffer-substring (line-beginning-position (point))))
    (newline))
  (indent-according-to-mode))

(provide 'smartparens-c)
