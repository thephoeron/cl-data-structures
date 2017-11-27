(in-package #:cl-data-structures)


(defgeneric at (container location))

(defgeneric (setf at) (new-value container location)
  (:generic-function-class insert!-function))

(defgeneric add (container location new-value)
  (:generic-function-class functional-add-function))

(defgeneric put (container item)
  (:generic-function-class functional-put-function))

(defgeneric near (container item maximal-distance))

(defgeneric add! (container location new-value)
  (:generic-function-class add!-function))

(defgeneric insert (container location new-value)
  (:generic-function-class functional-insert-function))

(defgeneric erase (container location)
  (:generic-function-class functional-erase-function))

(defgeneric erase-if (container location condition-fn)
  (:generic-function-class functional-erase-if-function))

(defgeneric erase-if! (container location condition-fn)
  (:generic-function-class erase-if!-function))

(defgeneric erase! (container location)
  (:generic-function-class erase!-function))

(defgeneric put! (container item)
  (:generic-function-class put!-function))

(defgeneric size (container))

(defgeneric update (container location new-value)
  (:generic-function-class functional-update-function))

(defgeneric update! (container location new-value)
  (:generic-function-class update!-function))

(defgeneric become-functional (container)
  (:method ((container functional)) container))

(defgeneric become-mutable (container)
  (:method ((container mutable)) container))

(defgeneric become-transactional (container))

(defgeneric become-lazy (container))

(defgeneric mutablep (container)
  (:method ((container mutable)) t)
  (:method ((container fundamental-container)) nil))

(defgeneric functionalp (container)
  (:method ((container functional)) t)
  (:method ((container fundamental-container)) nil))

(defgeneric transactionalp (container)
  (:method ((container transactional)) t)
  (:method ((container fundamental-container)) nil))

(defgeneric value (status))

(defgeneric found (status))

(defgeneric functional-counterpart (operation)
  (:method ((operation functional-function)) operation)
  (:method ((operation erase!-function)) #'erase)
  (:method ((operation erase-if!-function)) #'erase-if)
  (:method ((operation add!-function)) #'add)
  (:method ((operation insert!-function)) #'insert)
  (:method ((operation update!-function)) #'update))

(defgeneric destructive-counterpart (operation)
  (:method ((operation destructive-function)) operation)
  (:method ((operation functional-erase-function)) #'erase!)
  (:method ((operation functional-erase-if-function)) #'erase-if!)
  (:method ((operation functional-add-function)) #'add!)
  (:method ((operation functional-insert-function)) #'(setf at))
  (:method ((operation functional-update-function)) #'update!))

#|

Range releated functions.

|#

(defgeneric consume-front (range))

(defgeneric peek-front (range))

(defgeneric consume-back (range))

(defgeneric peek-back (range))

(defgeneric consume-some (range count))

(defgeneric chain (first-range &rest other-ranges))

(defgeneric drop-front (range count))

(defgeneric drop-back (range count))

(defgeneric morep (range))

(defgeneric clone (range))

(defgeneric whole-range (container))

#|

Algorithms

|#

(defgeneric on-each (function range)
  (:generic-function-class on-each-function)
  (:method (function (range fundamental-range))
    (apply-range-function range #'on-each :function function)))


(defclass forward-proxy-box-range (fundamental-forward-range)
  ((%original-range :initarg :original-range
                    :reader read-original-range)
   (%function :initarg :function
              :reader read-function)))


(defmethod clone ((range forward-proxy-box-range))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :function (read-function range)))


(defclass bidirectional-proxy-box-range (forward-proxy-box-range)
  ())


(defclass random-access-proxy-box-range (bidirectional-proxy-box-range)
  ())


(defgeneric on-each-proxy-range-from-range (range function)
  (:method :around ((range fundamental-range) function)
    (check-type function (or symbol function))
    (call-next-method))
  (:method ((range fundamental-forward-range) function)
    (make-instance 'forward-proxy-box-range
                   :original-range range
                   :function function))
  (:method ((range fundamental-bidirectional-range) function)
    (make-instance 'bidirectional-proxy-box-range
                   :original-range range
                   :function function))
  (:method ((range fundamental-random-access-range) function)
    (make-instance 'random-access-proxy-box-range
                   :original-range range
                   :function function)))


(defmethod apply-layer ((range fundamental-range)
                        (fn on-each-function)
                        &rest all &key function)
  (on-each-proxy-range-from-range range function))


(defgeneric change-each! (function range)
  (:generic-function-class transformation!-function))
