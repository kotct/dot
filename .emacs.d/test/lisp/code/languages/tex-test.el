(add-to-list 'load-path (concat (file-name-directory load-file-name) "../../../../lisp/code/"))

;; we have to require flyspell since the spy on flyspell-mode-on has to
;; override the actual function, not the other way around
(require 'flyspell)

(kotct/load-corresponding)

(describe "tex config"
  (describe "mode hook"
    :var (test-buffer)

    (before-each
      (setf test-buffer (generate-new-buffer "*kotct/tex-test-buffer*"))
      (set-buffer test-buffer))

    (after-each
      (kill-buffer test-buffer))

    (it "enables visual-line-mode"
      (tex-mode)
      (expect visual-line-mode :to-be-truthy))

    (describe "flyspell-mode"
      (spy-on #'executable-find :and-call-fake
              (lambda (command)
                (string= command "findme")))

      ;; we need this noop because the real flyspell-mode-on will
      ;; try to start ispell-program name (which of course won't work)
      (spy-on #'flyspell-mode-on :and-call-fake (lambda ()))

      (it "is enabled when ispell-program-name can be found"
        (let ((ispell-program-name "findme"))
          (tex-mode)
          (expect flyspell-mode :to-be-truthy)
          (expect #'executable-find :to-have-been-called)))

      (it "is not enabled when ispell-program-name cannot be found"
        (let ((ispell-program-name "DONTfindme"))
          (tex-mode)
          (expect flyspell-mode :not :to-be-truthy)
          (expect #'executable-find :to-have-been-called))))))
