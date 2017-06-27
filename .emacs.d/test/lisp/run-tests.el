(package-initialize)

(load-file (concat (file-name-directory load-file-name) "/dot-tests.el"))

(kotct/run-tests)
