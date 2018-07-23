;; make sure files always have a LF at EOF when saving
;; if you don't want this for a particular file, do
;; M-x set-variable RET require-final-newline RET nil RET
(setq-default require-final-newline t)

;; UTF-8 is a sane default
(prefer-coding-system 'utf-8)

(provide 'file-content)
