;; Enable EditorConfig mode
(editorconfig-mode +1)

(defun kotct/editorconfig-disable-smart-tabs-if-indent-tabs-mode-disabled (&optional hash)
  "Disables `smart-tabs-mode' if `indent-tabs-mode' is non-truthy.

HASH is passed in when `editorconfig-custom-hooks' is evaluated,
and is ignored."
  (if (not indent-tabs-mode)
      (progn
        (smart-tabs-mode -1))))

(add-hook 'editorconfig-custom-hooks 'kotct/editorconfig-disable-smart-tabs-if-indent-tabs-mode-disabled)

(defvar kotct/editorconfig-suppress-core-not-found-warning
  nil
  "If non-nil, warning on startup is suppressed.")

(defun kotct/editorconfig-check-for-core (&optional suppress)
  "Checks if EditorConfig C core is installed.

Debug warning is suppressed if SUPPRESS is non-nil."

  (if (not suppress)
      (if (not (executable-find editorconfig-exec-path))
          (message "EditorConfig C Core not found in `editorconfig-exec-path'."))))

(kotct/editorconfig-check-for-core kotct/editorconfig-suppress-core-not-found-warning)

(provide 'editorconfig-c)
