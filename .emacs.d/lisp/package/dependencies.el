(defvar dependency-list
      '(linum-off ;; turn linum off in certain modes
        ido-vertical-mode ;; use vertical menus for ido
        ido-grid-mode ;; use a grid selection menu for ido
        smex ;; ido-enabled M-x
        )
      "A list of all packages required for operation.")

(provide 'dependencies)
