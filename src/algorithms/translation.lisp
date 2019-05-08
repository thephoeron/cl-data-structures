(cl:in-package #:cl-data-structures.algorithms)


(defclass translation-function (on-each-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric translation (range dict &key key)
  (:generic-function-class translation-function)
  (:method (range dict &key (key #'identity))
    (apply-range-function range #'translation
                          :key key
                          :function (lambda (x)
                                      (bind (((:values result found)
                                              (cl-ds:at dict x)))
                                        (if found result x))))))
