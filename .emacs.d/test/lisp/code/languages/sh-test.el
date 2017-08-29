(add-to-list 'load-path (concat (file-name-directory load-file-name) "../../../../lisp/code/"))

(kotct/load-corresponding)

(describe "sh config"
  (it "adds a tab variable setter for `sh-basic-offset'"
    (expect (assoc 'sh-basic-offset kotct/tab-variable-setters) :to-be-truthy))
  (it "adds a tab variable setter for `sh-indentation'"
    (expect (assoc 'sh-indentation kotct/tab-variable-setters) :to-be-truthy))
  (it "sets the indentation for case labels to 0"
    (expect sh-indent-for-case-label :to-be 0))
  (it "sets the indentation for case alts to be one level"
    (expect sh-indent-for-case-alt :to-be '+)))
