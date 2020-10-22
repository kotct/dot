(require 'indentation)

;; Indent case labels at the same level as the case statement
(setf sh-indent-for-case-label 0)

;; Indent case alts to +
(setf sh-indent-for-case-alt '+)

(provide 'sh)
