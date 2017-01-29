;; enable linum globally
(global-linum-mode 1)

;; use a lambda to generate the linum formatting
;; this will use the minimum number of columns,
;; but also always right-align
(setq linum-format (lambda (line)
                     (let* ((w (length (number-to-string (count-lines (point-min) (point-max)))))
                            (thing (concat " %" (number-to-string (max 2 w)) "d ")))
                       (propertize (format thing line) 'face 'linum))))

;; disable linum in certain modes
(require 'linum-off)
(add-to-list 'linum-disabled-modes-list 'package-menu-mode)
(add-to-list 'linum-disabled-modes-list 'magit--mode)

(provide 'linum-c)
