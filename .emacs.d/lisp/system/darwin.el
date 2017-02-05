(defvar kotct/darwin-swap-command-option
  t
  "Determines whether `kotct/darwin-maybe-swap-command-option'
actually swaps command and option.")

;; This is intended to make the keyboard layout more friendly for
;; those who are used to a PC-like keyboard layout.  This makes C- and
;; M- more spread apart and makes the keyboard more ergonomic.
;;
;; It uses the `mac-command-modifier' and `mac-option-modifier'
;; builtins, and so will not affect Emacs installations that were
;; built on systems with these not enabled.
(defun kotct/darwin-maybe-swap-command-option ()
  "This function swaps the Command and Option modifiers on MacOS if
`kotct/darwin-swap-command-option' is truthy.

This only affects systems with `mac-command-modifier' and
`mac-option-modifier' built in. (i.e. NS builds, e.g. Homebrewed
Emacs)"
  (if kotct/darwin-swap-command-option
      (progn
        (setf mac-command-modifier 'meta)
        (setf mac-option-modifier 'super))))

(if (eq system-type 'darwin)
    (progn
      (kotct/darwin-maybe-swap-command-option)
      (when (memq window-system '(mac ns))
        (exec-path-from-shell-initialize))))

(provide 'darwin)
