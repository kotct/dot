(add-to-list 'load-path (concat (file-name-directory load-file-name) "../../../../lisp/code/"))

(kotct/load-corresponding)

(describe "sh config"
  (it "sets the indentation for case labels to 0"
    (expect sh-indent-for-case-label :to-be 0))
  (it "sets the indentation for case alts to be one level"
    (expect sh-indent-for-case-alt :to-be '+)))
