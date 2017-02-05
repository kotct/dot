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

(defvar kotct/darwin-load-path-from-shell
  t
  "Determines whether `kotct/darwin-maybe-load-path-from-shell'
actually loads the path from the shell.")

(defun kotct/darwin-maybe-load-path-from-shell ()
  "This function sets `exec-path' to the 'PATH' variable
generated by a shell if `kotct/darwin-load-path-from-shell' is
truthy.

In most sane environments, (i.e. Linux) the `exec-path' variable
is suitably close to or identical to the 'PATH' used in a
terminal.  On MacOS/Darwin environments, however, `exec-path' is
only the global value for the variable 'PATH'.

As such, access to things like Homebrew-installed packages is
restricted.  This function calls the shell and asks it to print
out the 'PATH' variable after it loads the `.profile'-related
stuff.  This also respects the user's shell decision."
  ;; Only run this once.  After you perform this step, TERM_PROGRAM is
  ;; set, and the path should already be okay.
  (if (not (getenv "TERM_PROGRAM"))
      ;; Ask the shell to print out its value for $PATH.
      (let ((path (shell-command-to-string "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
        ;; Set the PATH variable to this to match everything up.
        (setenv "PATH" path)
        ;; Split $PATH and store in `exec-path'.
        (setf exec-path (split-string (getenv "PATH") ":")))))

(if (eq system-type 'darwin)
    (progn
      (add-hook 'after-init-hook 'kotct/darwin-maybe-swap-command-option)
      (add-hook 'after-init-hook 'kotct/darwin-maybe-load-path-from-shell)))

(provide 'darwin)
