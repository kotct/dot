(kotct/load-corresponding)

(describe "kotct/check-editorconfig-props"
  (before-each
    (spy-on #'message))
  (describe "on a file with no corresponding editorconfig props"
    (describe "when kotct/warn-on-editorconfig-with-no-props is truthy"
      (it "prints a warning"
        (let* ((kotct/warn-on-editorconfig-with-no-props t)
                (temp-file-name (make-temp-file "tests"))
                (temp-file-buffer (create-file-buffer temp-file-name)))
          (progn
            (with-current-buffer temp-file-buffer
              (setf buffer-read-only t)
              (set-visited-file-name temp-file-name)
              (expect (buffer-file-name) :to-equal temp-file-name)
              (expect #'message :to-have-been-called)
              (expect #'message :to-have-been-called-with
                (format "EditorConfig mode is active for buffer %s, but no properties were applied." (buffer-name))))))))

    (describe "when kotct/warn-on-editorconfig-with-no-props is not truthy"
      (it "doesn't print a warning"))))
