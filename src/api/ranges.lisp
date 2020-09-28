(in-package :cl-data-structures)

(defclass empty-range (fundamental-forward-range)
  ()
  (:metaclass funcallable-standard-class))

(defmethod consume-front ((range empty-range))
  (values nil nil))

(defmethod peek-front ((range empty-range))
  (values nil nil))

(defmethod reset! ((range empty-range))
  range)

(defmethod clone ((range empty-range))
  (make (class-of range)))

(defmethod traverse ((range empty-range)
                           function)
  (ensure-functionf function)
  range)

(defmethod across ((range empty-range)
                         function)
  (ensure-functionf function)
  range)
