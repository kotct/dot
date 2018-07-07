;;  -*- lexical-binding: t; -*-

;;; The following keybindings are available when editing C# files using omnisharp:
;; C-RET: Do a refactoring at point. Prompt for which refactoring in the minibuffer.
;; M-RET: Same as C-RET
;; C-c o r: Intelligently rename the symbol at point, throughout project
;; M-.: Jump to definition of symbol at point
;; C-.: Find usages of symbol at point
;; C->: Find implementations of symbol at point
;; C-M-\\: Reformat the region


(defun kotct/omnisharp-install-server ()
  "Install omnisharp-roslyn, or, if using Arch Linux, make sure it is installed
and then install the correct wrapper."
  (interactive)
  (let ((os (shell-command-to-string "lsb_release -ds")))
    (if (string= os "\"Arch Linux\"\n")
        (if (= 0 (call-process-shell-command "pacman -Q msbuild-stable omnisharp-roslyn"))
            (progn
              (customize-save-variable 'omnisharp-server-executable-path "~/.emacs.d/bin/omnisharp")
              (message "Ready to roll! Do M-x omnisharp-mode to start OmniSharp."))
          (message "Looks like you're using Arch Linux!
Please install omnisharp-roslyn and msbuild-stable from the AUR.
When you're done, try running M-x kotct/omnisharp-install-server again."))
      ;; default to built-in installation
      (omnisharp-install-server nil))))

(defun kotct/omnisharp-maybe-start ()
  "Start omnisharp, or prompt user to install omnisharp-roslyn."
  (interactive)
  (require 'omnisharp)
  ;; this is approximately the check omnisharp does to find the server
  (let ((path (or omnisharp-server-executable-path
                  (omnisharp--server-installation-path t))))
    (if (f-exists-p path)
        (omnisharp-mode +1)
      (message "Heads up! For sick autocompletion and autodocs, you should install OmniSharp!
Do M-x kotct/omnisharp-install-server and we'll take care of it."))))

(add-hook 'csharp-mode-hook
          (lambda ()
            (auto-complete-mode -1)
            (company-mode +1)
            (kotct/omnisharp-maybe-start)))

(defvar omnisharp-company-active-map
  nil
  "Overriding `company-active-map' for omnisharp-mode.")

(with-eval-after-load 'company
  (setf omnisharp-company-active-map (copy-tree company-active-map))
  (dolist (key '("." "]"))
    (define-key omnisharp-company-active-map (kbd key)
      (lambda ()
        (interactive)
        (company-complete-selection)
        (insert key)
        (company-complete))))
  (dolist (key '("]" ")" ";" ">"))
    (define-key omnisharp-company-active-map (kbd key)
      (lambda ()
        (interactive)
        (company-complete-selection)
        (insert key)))))

(with-eval-after-load 'omnisharp
  (add-hook 'omnisharp-mode-hook
            (lambda ()
              (with-eval-after-load 'company
                (set (make-local-variable 'company-active-map) omnisharp-company-active-map))
              (eldoc-mode +1)
              (flycheck-mode +1)))

  (define-key omnisharp-mode-map (kbd "<C-RET>") #'omnisharp-run-code-action-refactoring)
  (define-key omnisharp-mode-map (kbd "<M-RET>") #'omnisharp-run-code-action-refactoring)
  (define-key omnisharp-mode-map (kbd "C-c o r") #'omnisharp-rename)
  (define-key omnisharp-mode-map (kbd "M-.") #'omnisharp-go-to-definition)
  (define-key omnisharp-mode-map (kbd "C-.") #'omnisharp-find-usages)
  (define-key omnisharp-mode-map (kbd "C->") #'omnisharp-find-implementations)
  (define-key omnisharp-mode-map (kbd "C-M-\\") #'omnisharp-code-format-region)

  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-omnisharp)))

(with-eval-after-load 'csharp-mode
  (sp-local-pair 'csharp-mode "{" "}"
                 :actions '(insert wrap autoskip navigate)
                 :unless '(sp-in-string-p)
                 :post-handlers '("[i]" ("||\n[i]" "RET" "C-j" newline))))

(provide 'csharp)
