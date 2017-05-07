;; disable tool bar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(provide 'frame-c)
