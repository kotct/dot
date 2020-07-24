(kotct/load-corresponding)

(describe "kotct/check-editorconfig-props"
  (describe "on a file with no corresponding editorconfig props"
    (describe "when kotct/warn-on-editorconfig-with-no-props is truthy"
      (it "prints a warning"))
    (describe "when kotct/warn-on-editorconfig-with-no-props is not truthy"
      (it "doesn't print a warning"))))
