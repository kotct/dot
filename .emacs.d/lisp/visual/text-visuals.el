;; Minor mode to provide visual feedback for
;; some operations.
(require 'volatile-highlights)
(volatile-highlights-mode +1)

;; Minor mode which displays search information
;; in the mode line.
(global-anzu-mode +1)

;; Minor mode which highlights TODO, FIXME, DONE, FAIL, etc.
(add-hook 'prog-mode-hook 'hl-todo-mode)

;; Sets the font name to NAME.
(defun kotct/font-set-name (name)
  "Sets the `:font' face attribute on the `default' face to match NAME."
  (set-face-attribute 'default nil :font name))

(defvar kotct/font-default-height
  12.0
  "The default font height.")

;; Sets the font height to what a user would expect their value to map to.
;;
;; Note that Emacs `:height' values are 10x what users would expect
;; (to allow for handling of one decimal place in the font size). We
;; multiply by 10 and floor to normalize things as best as possible.
(defun kotct/font-set-height (height)
  "Sets the `:height' face attribute on the `default' face to match HEIGHT."
  (interactive "nNew font height: ")
  (set-face-attribute 'default nil :height (floor (* 10.0 height))))

;; Set the font height to the default height.
(kotct/font-set-height kotct/font-default-height)

;; Any matching parenthesis is highlighted
(show-paren-mode +1)

(provide 'text-visuals)
