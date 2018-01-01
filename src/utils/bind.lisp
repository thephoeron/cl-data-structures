(in-package :metabang-bind)


(defbinding-form (:vectors :use-values-p nil :accept-multiple-forms-p t)
  `(cl-ds.utils:with-vectors ,(serapeum:batches variables 2)))


(defbinding-form (:hash-tables :use-values-p nil :accept-multiple-forms-p t)
  (let* ((tables (mapcar (lambda (x) (list (gensym) (first x))) variables))
         (at-arguments (mapcar (lambda (x) (list (gensym) (third x))) variables))
         (at-forms (mapcar (lambda (x container argument)
                             (list (second x)
                                   `(gethash ,(first argument)
                                             ,(first container))))
                           variables tables at-arguments)))
    `(serapeum:nest
      (let* (,@tables ,@at-arguments))
      (symbol-macrolet ,at-forms))))


(defbinding-form (:lazy :use-values-p nil :accept-multiple-forms-p t)
  `(cl-ds.utils:lazy-let ,variables))
