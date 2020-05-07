(cl:in-package #:cl-data-structures)


(defmacro mod-bind ((first &optional found value changed) form &body body)
  (alexandria:with-gensyms (!status)
    `(multiple-value-bind (,first ,!status) ,form
       (declare (ignorable ,first ,!status))
       (symbol-macrolet (,@(remove-if (lambda (x) (null (car x)))
                                      `((,found (found ,!status))
                                        (,value (value ,!status))
                                        (,changed (changed ,!status)))))
         ,@body))))


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


(defmacro assert-one-dimension (more)
  (once-only (more)
    `(unless (endp ,more)
       (error 'cl-ds:too-many-dimensions
              :format-control "Can't pass more then one dimension into one dimensional data structures."
              :value (1+ (length ,more))
              :bounds 1))))


(defmacro check-argument-bounds (argument expression)
  `(unless ,expression
     (error 'cl-ds:argument-value-out-of-bounds
            :argument ',argument
            :bounds ',expression
            :value ,argument)))
