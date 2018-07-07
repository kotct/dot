(kotct/load-corresponding)

(describe "file content config"
  (it "should require newlines at EOF"
    (expect require-final-newline :to-equal t)
    (expect (default-value 'require-final-newline) :to-equal t)))
