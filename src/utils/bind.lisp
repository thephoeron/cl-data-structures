(cl:in-package :metabang-bind)


(defbinding-form (:vectors :use-values-p nil :accept-multiple-forms-p t)
  `(cl-ds.utils:with-vectors ,(mapcar #'list variables values)))


(defbinding-form (:hash-table :use-values-p nil :accept-multiple-forms-p nil)
  (let* ((tables (list (gensym) values))
         (at-arguments (mapcar (lambda (x) (list (gensym) (second x))) variables))
         (at-forms (mapcar (lambda (x argument)
                             (list (first x)
                                   `(gethash ,(first argument)
                                             ,(first tables))))
                           variables at-arguments)))
    `(serapeum:nest
      (let* (,tables ,@at-arguments))
      (symbol-macrolet ,at-forms))))
