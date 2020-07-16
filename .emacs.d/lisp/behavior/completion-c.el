;;; M-x: ido-completed M-x
;;; C-c C-c M-x: normal M-x
;;;
;;; implicitly rebinds C-x C-f, C-x C-b, and similar functions to use ido
;;; different keybindings for ido navigation based on menu mode
;;; see docs for ido, ido-vertical-mode, and ido-grid-mode

(require 'ido)
(require 'ido-vertical-mode)
(require 'smex)

;;; ido
(ido-mode t)
(setf ido-enable-flex-matching t)

(defvar kotct/ido-current-menu-mode
  ;; set initially below, using kotct/ido-set-menu-mode
  nil
  "A symbol representing the current menu mode for ido.
Should be either 'grid, 'vertical, or 'normal.")

(defun kotct/ido-unset-menu-mode ()
  "Unsets the current menu mode for ido, to allow a new mode to be set."
  (cond ((eq kotct/ido-current-menu-mode 'grid)
         (ido-grid-mode -1))
        ((eq kotct/ido-current-menu-mode 'vertical)
         (ido-vertical-mode -1))))

(defun kotct/ido-set-menu-mode (mode)
  "Set ido to use a menu type MODE.

MODE is a symbol which can be grid (default), vertical, or normal."
  (kotct/ido-unset-menu-mode)
  (cond
   ((eq mode 'grid)
    (ido-grid-mode 1)
    (setf kotct/ido-current-menu-mode 'grid))
   ((eq mode 'vertical)
    (ido-vertical-mode 1)
    (setq ido-vertical-show-count t)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (setf kotct/ido-current-menu-mode 'vertical))
   (t ;; note that for 'normal we don't need to do anything
    (setf kotct/ido-current-menu-mode 'normal))))

;; default to grid mode
(kotct/ido-set-menu-mode 'grid)

;;; smex
(smex-initialize)
(global-set-key (kbd "M-x") #'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; keep things contained within ~/.emacs.d
(setf smex-save-file "~/.emacs.d/smex-items")

;;; autocomplete
(ac-config-default)

(provide 'completion-c)
