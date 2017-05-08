;;; (rebind) C-x C-+: increases font size by scaling `:height' frame parameter
;;; (rebind) C-x C-=: increases font size by scaling `:height' frame parameter
;;; (rebind) C-x C--: decreases font size by scaling `:height' frame parameter
;;; (rebind) C-x C-0: resets font size to `kotct/font-default-height'
;;; (rebind) C-s / C-r: isearch with all matches highlighted and match count in mode line
;;; (rebind) C-M-s / C-M-r: regexp isearch with all matches highlighted and match count in mode line
;;; (rebind) M-%: query-replace with all matches highlighted and match count in mode line
;;; (rebind) C-M-%: query-replace-regexp with all matches highlighted and match count in mode line

;; Minor mode to provide visual feedback for
;; some operations.
(require 'volatile-highlights)
(volatile-highlights-mode +1)

;; Minor mode which displays search information
;; in the mode line.
(global-anzu-mode +1)
(global-set-key [remap query-replace] #'anzu-query-replace)
(global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp)

;; Minor mode which highlights TODO, FIXME, DONE, FAIL, etc.
(add-hook 'prog-mode-hook 'hl-todo-mode)

(defvar kotct/font-default-height
  12.0
  "The default font height.")

;; Sets the font name to NAME.
(defun kotct/font-set-name (name)
  "Sets the `:font' face attribute on the `default' face to match NAME."
  (set-face-attribute 'default nil :font name))

;; Sets the font height to what a user would expect their value to map to.
;;
;; Note that Emacs `:height' values are 10x what users would expect
;; (to allow for handling of one decimal place in the font size). We
;; multiply by 10 and floor to normalize things as best as possible.
(defun kotct/font-set-height (height)
  "Sets the `:height' face attribute on the `default' face to match HEIGHT."
  (interactive "nNew font height: ")
  (set-face-attribute 'default nil :height (floor (* 10.0 height))))

(defvar kotct/font-rescale-factor
  1.0
  "The proportion that the font height change is multiplied by.")

(defun kotct/font-rescale-height (amount)
  "Sets the `:height' face attribute on the `default' face to
`kotct/font-rescale-factor' * AMOUNT + the current height."
  (let ((new-size (+ (* kotct/font-rescale-factor amount)
                     (/ (face-attribute 'default :height) 10.0))))
    (set-face-attribute 'default nil :height (floor (* 10.0 new-size)))))

(defun kotct/font-upscale-height () (interactive "*") (kotct/font-rescale-height +1.0))
(defun kotct/font-downscale-height () (interactive "*") (kotct/font-rescale-height -1.0))
(defun kotct/font-reset-height () (interactive "*") (kotct/font-set-height kotct/font-default-height))

(global-unset-key (kbd "C-x C-+"))
(global-unset-key (kbd "C-x C-="))
(global-unset-key (kbd "C-x C--"))
(global-unset-key (kbd "C-x C-0"))

(global-set-key (kbd "C-x C-+") #'kotct/font-upscale-height)
(global-set-key (kbd "C-x C-=") #'kotct/font-upscale-height)
(global-set-key (kbd "C-x C--") #'kotct/font-downscale-height)
(global-set-key (kbd "C-x C-0") #'kotct/font-reset-height)

;; Reset the font height to the default.
(kotct/font-reset-height)

;; Any matching parenthesis is highlighted.
(show-paren-mode +1)

(provide 'text-visuals)
