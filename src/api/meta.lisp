(in-package #:cl-data-structures)


(defclass functional-function ()
  ())


(defclass grow-function ()
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


(defgeneric grow-modification (operation container location new-value))


(defgeneric grow-bucket (operation container bucket location value
                         &key &allow-other-keys))


(defgeneric make-bucket (operation container bucket location value
                         &key &allow-other-keys))
