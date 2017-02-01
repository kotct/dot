;; set the default tab width to 4 chars
;; this is the reference variable for the tab width
(setf global-tab-width 4)

;; define automated tab-setting system
(defvar kotct/tab-variable-setters
  '((global-tab-width . #'set))
  "An alist of symbols representing variables and the method
used to set them to the tab width 
(almost always either #'set or #'set-default).")

(defmacro kotct/setf-tab (var)
  "Set the variable VAR to the tab width and keep it updated
if the tab width changes."
  `(progn
     (setf var global-tab-width)
     (setf kotct/tab-variable-setters (acons var #'set kotct/tab-variable-setters))))

(defmacro kotct/setq-default-tab (var)
  "Set default value of the variable VAR to the tab width 
and keep it updated if the tab width changes."
  `(progn
     (setq-default var global-tab-width)
     (setf kotct/tab-variable-setters (acons var #'set-default kotct/tab-variable-setters))))

(defun kotct/set-tab-width (width)
  (dolist (pair)
    (apply (cdr pair) (car pair) (list width))))

(setq-default-tab tab-width global-tab-width)
(setf-tab smie-indent-basic global-tab-width)

;; by default, don't use tabs
(setq-default indent-tabs-mode nil)

;; when you hit DEL on a tab, delete the whole tab, don't convert to spaces
(setf backward-delete-char-untabify-method nil)

(provide 'indentation)
