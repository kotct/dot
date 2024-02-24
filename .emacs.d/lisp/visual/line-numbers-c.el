(defun kotct/line-numbers--set-up-linum ()
  "Sets up `linum-mode' to run globally except in a few cases.")
  ;; (progn
  ;;   ;; enable linum globally
  ;;   (global-linum-mode 1)

  ;;   ;; use a lambda to generate the linum formatting
  ;;   ;; this will use the minimum number of columns,
  ;;   ;; but also always right-align
  ;;   (setq linum-format (lambda (line)
  ;;                        (let* ((w (length (number-to-string (count-lines (point-min) (point-max)))))
  ;;                               (thing (concat " %" (number-to-string (max 2 w)) "d ")))
  ;;                          (propertize (format thing line) 'face 'linum))))

    ;; ;; disable linum in certain modes
    ;; (require 'linum-off)
    ;; (add-to-list 'linum-disabled-modes-list 'package-menu-mode)
    ;; (add-to-list 'linum-disabled-modes-list 'magit--mode)))

(defun kotct/display-line-numbers--turn-off ()
  "Turns off `display-line-numbers-mode' for the current buffer."
  (display-line-numbers-mode -1))

(defun kotct/line-numbers--set-up-display-line-numbers ()
  "Sets up `display-line-numbers-mode'.

This mode was added in Emacs 26.1 and is recommended over `linum-mode' because
it generally works better."
  (progn
    ;; inhibit global-linum-mode
    ;; (global-linum-mode -1)
    (global-display-line-numbers-mode 1)

    ;; add some hooks to turn off display-line-numbers mode
    (dolist (hook '(dired-mode-hook
                    git-commit-mode-hook
                    magit-mode-hook
                    package-menu-mode-hook))
      (add-hook hook #'kotct/display-line-numbers--turn-off))))

(if (fboundp #'display-line-numbers-mode)
    (kotct/line-numbers--set-up-display-line-numbers)
  (kotct/line-numbers--set-up-linum))

(provide 'line-numbers-c)
