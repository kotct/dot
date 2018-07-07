;;; C-c C-f: find the current file as sudo

(defun kotct/sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::"
                         (ido-read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

(global-set-key (kbd "C-c C-f") #'kotct/sudo-edit)

(provide 'tramp-c)
