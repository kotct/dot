;; Make a bunch of extensions open as web mode things.
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsonp?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Don't indent Meta-HTML control blocks, and don't
;; apply any padding to <script> tags in HTML.
(setf web-mode-enable-control-block-indentation nil)
(setf web-mode-script-padding 0)

(add-hook 'web-mode-hook (lambda () (setf indent-tabs-mode t)))

;; add IDE-level JS support through tern
;; must have tern executable installed
;; easiest way is just to do `npm i -g tern`
(when (executable-find "tern")
  (add-hook 'web-mode-hook (lambda () (tern-mode +1)))
  (with-eval-after-load 'tern
    (tern-ac-setup)))

(provide 'web-c)
