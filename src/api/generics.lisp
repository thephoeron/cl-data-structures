(in-package #:cl-data-structures)


(defgeneric at (container location))

(defgeneric (setf at) (new-value container location))

(defgeneric add (container location new-value)
  (:generic-function-class add-function)
  (:method ((container fundamental-container) location new-value)
    (position-modification #'add container location :value new-value)))

(defgeneric add! (container location new-value))

(defgeneric insert (container location new-value)
  (:generic-function-class insert-function)
  (:method ((container fundamental-container) location new-value)
    (position-modification #'insert container location :value new-value)))

(defgeneric erase (container location)
  (:generic-function-class erase-function)
  (:method ((container fundamental-container) location)
    (position-modification #'erase container location)))

(defgeneric erase! (container location))

(defgeneric size (container))

(defgeneric update (container location new-value)
  (:generic-function-class update-function)
  (:method ((container fundamental-container) location new-value)
    (position-modification #'update container location :value new-value)))

(defgeneric update! (container location new-value))

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
