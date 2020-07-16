(require 'smie)

(defvar global-tab-width 4
  "The global tab width.
Defaults to 4.")

;; define automated tab-setting system
(defvar kotct/tab-variable-setters
  '((global-tab-width . setf))
  "An alist of symbols representing variables and the method
used to set them to the tab width
(almost always either setf or setq-default).")

(defmacro kotct/setf-tab (var)
  "Set the variable VAR to the tab width and keep it updated
if the tab width changes."
  `(progn
     (setf ,var global-tab-width)
     (setf kotct/tab-variable-setters (cons  '(,var . setf) kotct/tab-variable-setters))))

(defmacro kotct/setq-default-tab (var)
  "Set default value of the variable VAR to the tab width
and keep it updated if the tab width changes."
  `(progn
     (setq-default ,var global-tab-width)
     (setf kotct/tab-variable-setters (cons '(,var . setq-default) kotct/tab-variable-setters))))

(defmacro kotct/set-tab-width (width)
  "Set `global-tab-width' and all other associated tab width
variables in `kotct/tab-variable-setters' to WIDTH."
  (cons 'progn
        (mapcar (lambda (pair) (list (cdr pair) (car pair) width))
                kotct/tab-variable-setters)))

(kotct/setq-default-tab tab-width)
(kotct/setf-tab smie-indent-basic)

;; Set the base indent variable.
(kotct/setf-tab standard-indent)

;; by default, don't use tabs
(setq-default indent-tabs-mode nil)

;; when you hit DEL on a tab, delete the whole tab, don't convert to spaces
(setf backward-delete-char-untabify-method nil)

(provide 'indentation)
