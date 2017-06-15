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
