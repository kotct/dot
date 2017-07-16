(kotct/load-corresponding)

(describe "completion config"
  (describe "ido menu mode"
    :var (orig-mode)

    (before-all
      ; record original setting for later
      (setf orig-mode kotct/ido-current-menu-mode))

    (after-all
      ; restore original setting
      (setf kotct/ido-current-menu-mode orig-mode))

    (before-each
      (spy-on #'ido-grid-mode)
      (spy-on #'ido-vertical-mode))

    (describe "is unset correctly"
      (it "when using grid mode"
        (let ((kotct/ido-current-menu-mode 'grid))
          (kotct/ido-unset-menu-mode))

        (expect #'ido-grid-mode :to-have-been-called)
        (expect (spy-calls-args-for #'ido-grid-mode 0) :to-equal '(-1)))

      (it "when using vertical mode"
        (let ((kotct/ido-current-menu-mode 'vertical))
          (kotct/ido-unset-menu-mode))

        (expect #'ido-vertical-mode :to-have-been-called)
        (expect (spy-calls-args-for #'ido-vertical-mode 0) :to-equal '(-1))))

    (describe "is set correctly"
      (it "to grid mode"
        (let ((kotct/ido-current-menu-mode 'normal))
          (kotct/ido-set-menu-mode 'grid))

        (expect #'ido-grid-mode :to-have-been-called)
        (expect (spy-calls-args-for #'ido-grid-mode 0) :to-equal '(+1)))

      (it "to vertical mode"
        (let ((kotct/ido-current-menu-mode 'normal))
          (kotct/ido-set-menu-mode 'vertical))

        (expect #'ido-vertical-mode :to-have-been-called)
        (expect (spy-calls-args-for #'ido-vertical-mode 0) :to-equal '(+1))))))
