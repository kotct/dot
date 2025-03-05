;; when you hit DEL on a tab, delete the whole tab, don't convert to spaces
(setf backward-delete-char-untabify-method nil)

(provide 'indentation)
