(require 'fish-mode)

(smart-tabs-add-language-support fish fish-mode-hook
  ((fish-indent-line . standard-indent)
   (fish-indent-region . standard-indent)))

(smart-tabs-insinuate 'fish)

(add-hook 'fish-mode-hook (lambda () (setf indent-tabs-mode t)))

(provide 'fish)
