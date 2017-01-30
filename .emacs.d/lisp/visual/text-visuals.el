;; Minor mode to provide visual feedback for
;; some operations.
(require 'volatile-highlights)
(volatile-highlights-mode +1)

;; Minor mode which displays search information
;; in the mode line.
(global-anzu-mode +1)

;; Any matching parenthesis is highlighted
(show-paren-mode +1)

(provide 'text-visuals)
