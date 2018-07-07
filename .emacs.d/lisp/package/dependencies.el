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
    avy ;; immediately jump to any visible character
    avy-zap ;; zap-to-char but harnessing avy
    editorconfig ;; editorconfig plugin
    hl-todo ;; highlight TODO, FIXME, FAIL, etc.
    highlight-symbol ;; navigate between and highlight symbols
    web-mode ;; for editing web files
    elixir-mode ;; for editing Elixir code
    rust-mode ;; for editing Rust code
    yaml-mode ;; for editing YAML files
    buttercup ;; for tests
    smartparens ;; for dealing with paired control flow symbols
    gitignore-mode ;; for editing .gitignore files
    gitattributes-mode ;; for editing .gitattributes files
    gitconfig-mode ;; for editing .git/config files
    fish-mode ;; for editing fish shell configuration files
    go-mode ;; for editing go code
    go-autocomplete ;; AC support for go

    ;; THEMES
    solarized-theme)
  "A list of all packages required for operation.")

(with-eval-after-load 'package
  (setf package-pinned-packages
        '(;; stable highlight-symbol is very old and VERY LOUD
          (highlight-symbol . "melpa"))))

(provide 'dependencies)
