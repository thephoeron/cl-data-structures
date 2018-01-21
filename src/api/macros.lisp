(in-package #:cl-data-structures)


(defmacro mod-bind ((first &optional found value) form &body body)
  (alexandria:with-gensyms (!status)
    `(multiple-value-bind (,first ,!status) ,form
       (symbol-macrolet (,@(remove-if (lambda (x) (null (car x)))
                                      `((,found (found ,!status))
                                        (,value (value ,!status)))))
         ,@body))))


(defmacro traverse-through ((traversable var) &body body)
  (with-gensyms (!callback)
    `(flet ((,!callback (,var)
              ,@body))
       (declare (dynamic-extent (function ,!callback)))
       (let ((cl-ds:*traverse-callback* (function ,!callback)))
         (declare (special cl-ds:*traverse-callback*))
         (cl-ds:traverse cl-ds:*traverse-callback* ,traversable)))))


(metabang.bind::defbinding-form (:at
                                 :use-values-p nil
                                 :accept-multiple-forms-p nil)
  (let* ((container (list (gensym) values))
         (variables metabang.bind::variables)
         (arguments (mapcar (lambda (x) (list (gensym) (second x))) variables))
         (symbols (mapcar #'first variables))
         (forms (mapcar (lambda (x argument) (list x `(cl-ds:at ,(first container)
                                                           ,(first argument))))
                        symbols arguments)))
    `(serapeum:nest
      (let* (,container ,@arguments))
      (symbol-macrolet ,forms))))


(metabang.bind::defbinding-form (:modification
                                 :use-values-p nil
                                 :accept-multiple-forms-p nil)
  (multiple-value-bind (bindings ignores)
      (metabang.bind.developer:bind-fix-nils metabang.bind::variables)
    (declare (ignore bindings))
    `(mod-bind ,metabang.bind::variables ,values
       (declare (ignore ,@ignores)))))
