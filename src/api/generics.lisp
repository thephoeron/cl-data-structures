(in-package #:cl-data-structures)


(defgeneric at (container location))

(defgeneric (setf at) (new-value container location)
  (:generic-function-class insert!-function)
  (:method (new-value (container mutable) location)
    (let ((status (nth-value
                   1
                   (position-modification #'(setf at)
                                          container
                                          location
                                          :value new-value))))
      (values new-value status))))

(defgeneric add (container location new-value)
  (:generic-function-class functional-add-function)
  (:method ((container fundamental-container) location new-value)
    (position-modification #'add container location :value new-value)))

(defgeneric add! (container location new-value)
  (:generic-function-class add!-function)
  (:method ((container mutable) location new-value)
    (position-modification #'add! container location :value new-value)))

(defgeneric insert (container location new-value)
  (:generic-function-class functional-insert-function)
  (:method ((container fundamental-container) location new-value)
    (position-modification #'insert container location :value new-value)))

(defgeneric erase (container location)
  (:generic-function-class functional-erase-function)
  (:method ((container fundamental-container) location)
    (position-modification #'erase container location)))

(defgeneric erase-if (container location condition-fn)
  (:generic-function-class functional-erase-if-function)
  (:method ((container fundamental-container) location condition-fn)
    (position-modification #'erase-if container location :condition-fn condition-fn)))

(defgeneric erase-if! (container location condition-fn)
  (:generic-function-class erase-if!-function)
  (:method ((container fundamental-container) location condition-fn)
    (position-modification #'erase-if! container location :condition-fn condition-fn)))

(defgeneric erase! (container location)
  (:generic-function-class erase!-function)
  (:method ((container fundamental-container) location)
    (position-modification #'erase! container location)))

(defgeneric size (container))

(defgeneric update (container location new-value)
  (:generic-function-class functional-update-function)
  (:method ((container fundamental-container) location new-value)
    (position-modification #'update container location :value new-value)))

(defgeneric update! (container location new-value)
  (:generic-function-class update!-function)
  (:method ((container mutable) location new-value)
    (position-modification #'update! container location :value new-value)))

(defgeneric become-functional (container)
  (:method ((container functional)) container))

(defgeneric become-mutable (container)
  (:method ((container mutable)) container))

(defgeneric become-transactional (container))

(defgeneric become-lazy (container)
  (:method ((container lazy)) container))

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
