(defvar kotct/current-theme
  nil
  "The currently enabled theme.")

(defun kotct/switch-to-theme (theme)
  "Enable the theme THEME. Disable the current theme."
  (if kotct/current-theme (disable-theme kotct/current-theme))
  (if (member theme custom-known-themes)
      (enable-theme theme)
    (load-theme theme 'no-confirm))
  (setf kotct/current-theme theme)
  (cond ((or (string= kotct/current-theme 'solarized-dark)
	     (string= kotct/current-theme 'solarized-light))
	 (progn
	   (setf x-underline-at-descent-line t)))
	(t nil)))

(kotct/switch-to-theme 'solarized-dark)

(provide 'theme)
