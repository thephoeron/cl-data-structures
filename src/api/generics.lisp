(in-package #:cl-data-structures)


(defgeneric at (container location))

(defgeneric (setf at) (new-value container location)
  (:documentation "@b(Mutable API:) Destructively insert/replace element in the CONTAINER at LOCATION with NEW-VALUE."))

(defgeneric add (container location new-value))

(defgeneric add! (container location new-value))

(defgeneric insert (container location new-value))

(defgeneric erase (container location))

(defgeneric erase! (container location))

(defgeneric size (container))

(defgeneric update (container location new-value))

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

(defgeneric emptyp (container)
  (:method ((container fundamental-container)) (zerop (size container))))

(defgeneric value (status))

(defgeneric found (status))
