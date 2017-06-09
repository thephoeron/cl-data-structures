(in-package #:cl-data-structures)


(defmacro mod-bind ((first &optional found value) form &body body)
  "Macro, provides multiple-value-bind like syntax for modification operations."
  (alexandria:with-gensyms (!status)
    `(multiple-value-bind (,first ,!status) ,form
       (symbol-macrolet (,@(remove-if (lambda (x) (null (car x)))
                                      `((,found (found ,!status))
                                        (,value (value ,!status)))))
         ,@body))))
