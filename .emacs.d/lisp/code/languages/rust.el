(require 'indentation)

;; Don't indent `where' clauses in Rust.
(setf rust-indent-where-clause nil)

(provide 'rust)
