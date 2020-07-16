(require 'rust-mode)

(require 'indentation)

;; Set `rust-indent-offset' to the `global-tab-width' value.
(kotct/setq-default-tab rust-indent-offset)

;; Add language support for Rust.
(smart-tabs-add-language-support rust rust-mode-hook
  ((rust-mode-indent-line . rust-indent-offset)))

;; Use `indent-tabs-mode' in `rust-mode'
(add-hook 'rust-mode-hook (lambda () (setf indent-tabs-mode t)))

;; Turn on Rust support in Smart Tabs.
(smart-tabs-insinuate 'rust)

;; Don't indent `where' clauses in Rust.
(setf rust-indent-where-clause nil)

(provide 'rust)
