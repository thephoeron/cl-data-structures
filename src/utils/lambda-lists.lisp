(in-package #:cl-data-structures.utils)


(eval-always
  (defun method-lambda-list-to-function-lambda-list (lambda-list)
    (~>> lambda-list
         (mapcar (lambda (x) (if (listp x) (car x) x))))))


(eval-always
  (defun lambda-list-to-bindings (lambda-list)
    (~>> lambda-list
         (remove-if (rcurry #'member lambda-list-keywords))
         method-lambda-list-to-function-lambda-list)))


(eval-always
  (defun lambda-list-to-call-form (function-symbol lambda-list)
    (let* ((rest nil)
           (symbols
             (iterate
               (with mode = nil)
               (for symbol-or-list in lambda-list)
               (for symbol = (if (listp symbol-or-list)
                                 (car symbol-or-list)
                                 symbol-or-list))
               (for keyword = (car (member symbol-or-list
                                           lambda-list-keywords)))
               (if keyword
                   (setf mode keyword)
                   (cond
                     ((eql mode 'cl:&key) (progn
                                            (collect (make-keyword symbol))
                                            (collect symbol)))
                     ((eql mode 'cl:&aux) nil)
                     ((eql mode 'cl:&rest) (setf rest symbol))
                     (t (collect symbol)))))))
      (if (null rest)
          (cons function-symbol symbols)
          `(apply (function ,function-symbol)
                  ,@symbols
                  ,rest)))))
