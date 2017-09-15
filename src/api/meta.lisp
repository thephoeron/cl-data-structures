(in-package #:cl-data-structures)


(defclass functional-function ()
  ())


(defclass destructive-function ()
  ())


(defclass grow-function ()
  ())


(defclass shrink-function ()
  ())


(defclass insert-function (closer-mop:standard-generic-function
                           functional-function
                           grow-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass update-function (closer-mop:standard-generic-function
                           functional-function
                           grow-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass add-function (closer-mop:standard-generic-function
                        functional-function
                        grow-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass erase-function (closer-mop:standard-generic-function
                          functional-function
                          shrink-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass insert!-function (closer-mop:standard-generic-function
                            destructive-function
                            grow-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass update!-function (closer-mop:standard-generic-function
                            destructive-function
                            grow-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass add!-function (closer-mop:standard-generic-function
                         destructive-function
                         grow-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass erase!-function (closer-mop:standard-generic-function
                           destructive-function
                           shrink-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

