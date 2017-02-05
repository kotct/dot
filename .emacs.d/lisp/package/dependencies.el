(defvar kotct/dependency-list
  '(linum-off ;; turn linum off in certain modes
    ido-vertical-mode ;; use vertical menus for ido
    ido-grid-mode ;; use a grid selection menu for ido
    smex ;; ido-enabled M-x
    anzu ;; nicer search environment
    volatile-highlights ;; highlight changed areas
    buffer-move ;; easily move buffers between windows
    expand-region ;; expand region to successively larger sexps
    magit ;; make Git and Emacs both way cooler
    auto-complete ;; code autocompletion
    markdown-mode ;; major mode for markdown
    smart-tabs-mode ;; indentation with tabs and spaces
    ace-jump-mode ;; immediately jump to any visible character
    ace-jump-zap ;; zap-to-char but harnessing ace-jump
    editorconfig ;; editorconfig plugin
    hl-todo ;; highlight TODO, FIXME, FAIL, etc.
    highlight-symbol ;; navigate between and highlight symbols
    web-mode ;; for editing web files
    exec-path-from-shell ;; for fixing and unifying environment variables on macOS

    ;; THEMES
    solarized-theme)
  "A list of all packages required for operation.")

(provide 'dependencies)
