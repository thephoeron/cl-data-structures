(in-package #:cl-data-structures)


(defmacro mod-bind ((first &optional found value) form &body body)
  (alexandria:with-gensyms (!status)
    `(multiple-value-bind (,first ,!status) ,form
       (symbol-macrolet (,@(remove-if (lambda (x) (null (car x)))
                                      `((,found (found ,!status))
                                        (,value (value ,!status)))))
         ,@body))))


(defmacro transaction ((binding instance) &body operations)
  (alexandria:once-only (instance)
    `(let ((,binding (become-transactional ,instance)))
       ,@operations
       (cond ((functionalp ,instance)
              (become-functional ,binding))
             ((transactionalp ,instance)
              ,binding)
             (t (become-mutable ,instance))))))


(defmacro traverse-through ((traversable var) &body body)
  (with-gensyms (!callback)
    `(flet ((,!callback (,var)
              ,@body))
       (declare (dynamic-extent (function ,!callback)))
       (let ((cl-ds:*traverse-callback* (function ,!callback)))
         (declare (special cl-ds:*traverse-callback*))
         (cl-ds:traverse cl-ds:*traverse-callback* ,traversable)))))


(metabang.bind::defbinding-form (:at :use-values-p nil :accept-multiple-forms-p t)
  (multiple-value-bind (vars ignores)
      (metabang.bind.developer:bind-fix-nils metabang.bind::variables)
    (let* ((containers (mapcar (lambda (x) (list (gensym) (first x))) vars))
           (at-arguments (mapcar (lambda (x) (list (gensym) (third x))) vars))
           (at-forms (mapcar (lambda (x container argument) (list (second x)
                                                             `(cl-ds:at ,(first container)
                                                                        ,(first argument))))
                             vars containers at-arguments)))
      `(serapeum:nest
        (let* (,@containers ,@at-arguments))
        (symbol-macrolet ,at-forms)))))


(metabang.bind::defbinding-form (:modification
                                 :use-values-p nil
                                 :accept-multiple-forms-p nil)
  (multiple-value-bind (bindings ignores)
      (metabang.bind.developer:bind-fix-nils metabang.bind::variables)
    `(mod-bind ,bindings ,values)))
