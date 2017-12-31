(in-package :metabang-bind)


(defbinding-form (:vectors :use-values-p nil :accept-multiple-forms-p t)
  (multiple-value-bind (vars ignores)
      (metabang.bind.developer:bind-fix-nils variables)
    `(cl-ds.utils:with-vectors ,(serapeum:batches vars 2))))


(defbinding-form (:hash-tables :use-values-p nil :accept-multiple-forms-p t)
  (multiple-value-bind (vars ignores)
      (metabang.bind.developer:bind-fix-nils metabang.bind::variables)
    (let* ((tables (mapcar (lambda (x) (list (gensym) (first x))) vars))
           (at-arguments (mapcar (lambda (x) (list (gensym) (third x))) vars))
           (at-forms (mapcar (lambda (x container argument) (list (second x)
                                                             `(gethash ,(first argument)
                                                                       ,(first container))))
                             vars tables at-arguments)))
      `(serapeum:nest
        (let* (,@tables ,@at-arguments))
        (symbol-macrolet ,at-forms)))))
