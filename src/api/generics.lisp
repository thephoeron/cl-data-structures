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

(defmethod alexandria:emptyp ((container fundamental-container))
  (zerop (size container)))

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

(defgeneric whole-range (container))

#|

Algorithms

|#

(defgeneric on-each (function range)
  (:generic-function-class layer-function))


(defgeneric change-each! (function range)
  (:generic-function-class transformation!-function))
