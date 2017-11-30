(in-package #:cl-data-structures)


(defclass functional-function ()
  ())


(defclass destructive-function ()
  ())


(defclass grow-function ()
  ())


(defclass shrink-function ()
  ())


(defclass insert-function (grow-function)
  ())


(defclass update-function (grow-function)
  ())


(defclass add-function (grow-function)
  ())


(defclass erase-function (shrink-function)
  ())


(defclass erase-if-function (shrink-function)
  ())


(defclass put-function (grow-function)
  ())


(defclass functional-insert-function (closer-mop:standard-generic-function
                                      functional-function
                                      insert-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-update-function (closer-mop:standard-generic-function
                                      functional-function
                                      update-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-add-function (closer-mop:standard-generic-function
                                   functional-function
                                   add-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-erase-function (closer-mop:standard-generic-function
                                     functional-function
                                     erase-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-erase-if-function (closer-mop:standard-generic-function
                                        functional-function
                                        erase-if-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-put-function (closer-mop:standard-generic-function
                                   functional-function
                                   put-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass insert!-function (closer-mop:standard-generic-function
                            destructive-function
                            insert-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass update!-function (closer-mop:standard-generic-function
                            destructive-function
                            update-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass add!-function (closer-mop:standard-generic-function
                         destructive-function
                         add-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass erase-if!-function (closer-mop:standard-generic-function
                              destructive-function
                              erase-if-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass erase!-function (closer-mop:standard-generic-function
                           destructive-function
                           erase-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass put!-function (closer-mop:standard-generic-function
                         destructive-function
                         put-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric grow-bucket (operation container bucket location
                         &rest all
                         &key &allow-other-keys))


(defgeneric shrink-bucket (operation container bucket location
                           &rest all
                           &key &allow-other-keys))


(defgeneric make-bucket (operation container location
                         &rest all
                         &key &allow-other-keys))


(defgeneric grow-bucket! (operation container bucket location
                          &rest all
                          &key &allow-other-keys))


(defgeneric shrink-bucket! (operation container bucket location
                            &rest all
                            &key &allow-other-keys))


(defgeneric position-modification (operation
                                   container
                                   location
                                   &rest all
                                   &key &allow-other-keys))


(defgeneric apply-layer (range function &rest all &key &allow-other-keys))


(defclass range-function (closer-mop:standard-generic-function)
  ())


(defclass layer-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass transformation!-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass aggregation-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric make-state (aggregation-function))


(defgeneric aggregate (function state element))


(defgeneric state-result (function state))


(defgeneric apply-range-function (range function
                                  &rest all
                                  &key &allow-other-keys))


(defgeneric apply-aggregation-function (range function
                                        &rest all &key &allow-other-keys))
