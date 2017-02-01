;; Enable EditorConfig mode
(editorconfig-mode +1)

(defun kotct/editorconfig--disable-smart-tabs-if-indent-tabs-mode-disabled (&optional hash)
  "Disables `smart-tabs-mode' if `indent-tabs-mode' is non-truthy."
  (if (not indent-tabs-mode)
      (progn
        (smart-tabs-mode -1))))

(add-hook 'editorconfig-custom-hooks 'kotct/editorconfig--disable-smart-tabs-if-indent-tabs-mode-disabled)

(provide 'editorconfig-c)
