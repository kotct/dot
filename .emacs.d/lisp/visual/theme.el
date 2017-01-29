(defvar current-theme
  nil
  "The currently enabled theme.")

(defun switch-to-theme (theme)
  "Enable the theme THEME. Disable the current theme."
  (if current-theme (disable-theme current-theme))
  (if (member theme custom-known-themes)
      (enable-theme theme)
    (load-theme theme 'no-confirm))
  (setf current-theme theme))

(switch-to-theme 'solarized-dark)

(provide 'theme)
