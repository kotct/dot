;; -*- lexical-binding: t; -*-

(defvar kotct/current-theme
  nil
  "The currently enabled theme.")

(defvar kotct/deferred-theme
  nil
  "A theme that was not able to be loaded on init (probably because
we don't yet have a frame) and has been deferred to be loaded
later by `kotct/theme-callback'.")

(defun kotct/theme-callback (frame)
  "Can be called with a FRAME to select the frame and load `kotct/deferred-theme'.

Also removes itself from `after-make-frame-functions', if present."
  (remove-hook 'after-make-frame-functions #'kotct/theme-callback)
  ;; we must select the frame, so the theme can
  ;; access it via (selected-frame)
  (with-selected-frame frame
    (kotct/switch-to-theme (car kotct/deferred-theme) (cdr kotct/deferred-theme)))
  (setf kotct/deferred-theme nil))

(defun kotct/switch-to-theme (theme &optional force-load)
  "Enable the theme THEME. Disable the current theme.

If FORCE-LOAD is set, THEME is always loaded."
  (if (string= (terminal-name (frame-terminal)) "initial_terminal")
      ;; defer loading until after we have a frame
      (progn
        (setf kotct/deferred-theme (cons theme force-load))
        (add-hook 'after-make-frame-functions #'kotct/theme-callback))
    (if kotct/current-theme (disable-theme kotct/current-theme))
    (if (and (not force-load)
             (member theme custom-known-themes))
        (enable-theme theme)
      (load-theme theme 'no-confirm))
    (setf kotct/current-theme theme)
    ;; make specific theme customizations
    (cond ((or (eq kotct/current-theme 'solarized-dark)
               (eq kotct/current-theme 'solarized-light))
           (progn
             (setf x-underline-at-descent-line t))))))

(kotct/switch-to-theme 'solarized-dark)

(provide 'theme)
