(in-package #:cl-data-structures)


(defmacro mod-bind ((str &optional found value) form &body body)
  "Macro, provides multiple-value-bind like syntax for modification operations."
  (alexandria:with-gensyms (!status)
    `(multiple-value-bind (,str ,!status) ,form
       (symbol-macrolet (,@(remove-if (lambda (x) (null (car x)))
                                      `((,found (found ,!status))
                                        (,value (value ,!status)))))
         ,@body))))
