(kotct/load-corresponding)

(describe "sh config"
  (it "adds a tab variable setter for `sh-basic-offset'"
    (expect (assoc 'sh-basic-offset kotct/tab-variable-setters) :to-be-truthy))
  (it "adds a tab variable setter for `sh-indentation'"
    (expect (assoc 'sh-indentation kotct/tab-variable-setters) :to-be-truthy)))
