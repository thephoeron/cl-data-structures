(in-package #:cl-data-structures.algorithms)


(defclass count-elements-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric count-elements (range)
  (:generic-function-class count-elements-function)
  (:method (range)
    (apply-range-function range #'count-elements :key #'identity)))


(defmethod cl-ds.alg.meta:make-state ((function count-elements-function) &rest all)
  (list 0))


(defmethod aggregate ((function count-elements-function) state element)
  (incf (car state))
  state)


(defmethod state-result ((function count-elements-function) state)
  (car state))


(defclass count-elements-if-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric count-elements-if (range predicate &key key)
  (:generic-function-class count-elements-if-function)
  (:method (range predicate &key (key #'identity))
    (apply-range-function range #'count-elements-if
                          :key key :predicate predicate)))


(defmethod cl-ds.alg.meta:make-state ((function count-elements-if-function) &key predicate
                                      &allow-other-keys)
  (list* predicate 0))


(defmethod aggregate ((function count-elements-if-function) state element)
  (bind (((predicate . count) state)
         (should-count (funcall predicate element)))
    (if should-count
        (incf (cdr state))))
  state)


(defmethod state-result ((function count-elements-if-function) state)
  (cdr state))
