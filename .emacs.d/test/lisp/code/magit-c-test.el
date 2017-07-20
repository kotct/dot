(kotct/load-corresponding)

(describe "magit config"
  (describe "popups"
    :var (magit-sign-option)
    (before-all
      (require 'magit)
      (setf magit-sign-option '(?S "Sign using gpg" "--gpg-sign=" magit-read-gpg-secret-key nil)))
    (describe "merge popup"
      (it "includes sign option"
        (expect (member magit-sign-option (plist-get magit-merge-popup :options)) :to-be-truthy)))
    (describe "rebase popup"
      (it "includes sign option"
        (expect (member magit-sign-option (plist-get magit-rebase-popup :options)) :to-be-truthy))))

  (kotct/it-binds magit-status "C-x m"))
