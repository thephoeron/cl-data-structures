(in-package #:cl-data-structures)


(defclass functional-function ()
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


(defgeneric position-modification (operation container location &key &allow-other-keys))


(defgeneric position-erasure (operation container location))


(defgeneric grow-bucket (operation container bucket location
                         &key &allow-other-keys))


(defgeneric shrink-bucket (operation container bucket location
                           &key &allow-other-keys))


(defgeneric make-bucket (operation container bucket location
                         &key &allow-other-keys))
