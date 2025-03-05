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

  (if (and (not suppress) (boundp 'editorconfig-exec-path))
      (if (not (executable-find editorconfig-exec-path))
          (message "EditorConfig C Core not found in `editorconfig-exec-path'."))))

(kotct/editorconfig-check-for-core kotct/editorconfig-suppress-core-not-found-warning)

(defvar kotct/warn-on-editorconfig-with-no-props
  t
  "If not false-y, warn when editing a file with no editorconfig properties
applying, indicating that the user is stuck with Emacs' default style for the
active buffer.")

(defun kotct/check-editorconfig-props (props)
  "Warn the user if a file is being edited with EditorConfig active but no
properties were applied.

The argument PROPS is a property hash table provided by the EditorConfig
package, and contains key-value pairs corresponding to the user's configured
style for the current buffer.  If this hash table is empty, no properties were
applied to the buffer, so the user is at the whims of Emacs' default style for
the active mode, which may not be desirable."

  (if kotct/warn-on-editorconfig-with-no-props
      (let ((fn (buffer-file-name))
            (bn (buffer-name)))
        (if (and fn (= 0 (hash-table-count props)))
            (message (format "EditorConfig mode is active for buffer %s, but no properties were applied." bn))))))

(add-hook #'editorconfig-after-apply-functions #'kotct/check-editorconfig-props)

(provide 'editorconfig-c)
