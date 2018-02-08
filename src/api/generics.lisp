(in-package #:cl-data-structures)

(defgeneric frozenp (obj))

(defgeneric freeze! (obj))

(defgeneric melt! (obj))

(defun ensure-not-frozen (obj)
  (when (frozenp obj)
    (error 'ice-error :test "Trying to change frozen object!")))

(defmethod position-modification :before (operation
                                          container
                                          location
                                          &rest all
                                          &key &allow-other-keys)
  (declare (ignore operation location all))
  (ensure-not-frozen container))

(defgeneric at (container location))

(defgeneric (setf at) (new-value container location)
  (:generic-function-class insert!-function))

(defgeneric add (container location new-value)
  (:generic-function-class functional-add-function))

(defgeneric put (container item)
  (:generic-function-class functional-put-function))

(defgeneric take-out! (container)
  (:generic-function-class take-out!-function))

(defgeneric take-out (container)
  (:generic-function-class functional-take-out-function))

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

(defgeneric update-if (container location new-value condition-fn)
  (:generic-function-class functional-update-if-function))

(defgeneric update! (container location new-value)
  (:generic-function-class update!-function))

(defgeneric update-if! (container location new-value condition-fn)
  (:generic-function-class update-if!-function))

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
  (:method ((operation take-out!-function)) #'functional-take-out-function)
  (:method ((operation update!-function)) #'update))

(defgeneric destructive-counterpart (operation)
  (:method ((operation destructive-function)) operation)
  (:method ((operation functional-erase-function)) #'erase!)
  (:method ((operation functional-erase-if-function)) #'erase-if!)
  (:method ((operation functional-add-function)) #'add!)
  (:method ((operation functional-insert-function)) #'(setf at))
  (:method ((operation functional-take-out-function)) #'take-out!)
  (:method ((operation functional-update-function)) #'update!))

(defgeneric empty-clone (container))

(defgeneric traverse (function object)
  (:method (function (object sequence))
    (map nil function object)))

(defgeneric special-traverse (object)
  (:method (object)
    (traverse *traverse-callback* object)))

(defgeneric transaction (operation object location &rest args))

#|

Range releated functions.

|#

(defgeneric consume-front (range))

(defgeneric peek-front (range))

(defgeneric (setf peek-front) (new-value range))

(defgeneric consume-back (range))

(defgeneric peek-back (range))

(defgeneric (setf peek-back) (new-value range))

(defgeneric drop-front (range count))

(defgeneric drop-back (range count))

(defgeneric morep (range))

(defgeneric clone (range))

(defgeneric whole-range (container))

(defgeneric reset! (obj)
  (:method :before ((obj mutable))
    (when (frozenp obj)
      (error 'ice-error :text "Can't reset frozen containers."))))
