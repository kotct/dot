(require 'indentation)
(require 'rust-mode)

;; Don't indent `where' clauses in Rust.
(setf rust-indent-where-clause nil)

(provide 'rust)
