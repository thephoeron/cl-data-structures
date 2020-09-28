(in-package :cl-data-structures.common)

(defclass lazy-range ()
  ((%container :initarg :container
               :reader read-container)
   (%range :initarg :range
           :reader read-range
           :type (or (-> () cl-ds:fundamental-range)
                     cl-ds:fundamental-range)))
  (:metaclass funcallable-standard-class))

(defun force-lazy-range (range)
  (declare (type lazy-range range))
  (with-slots (%range %container) range
    (force-version %container)
    (when (functionp %range)
      (setf %range (funcall %range)))))

(defmacro make-lazy-range (class container range)
  `(make-instance ',class
                  :container ,container
                  :range (lambda () ,range)))

(defclass lazy-forward-range (cl-ds:fundamental-forward-range
                              lazy-range)
  ()
  (:metaclass funcallable-standard-class))

(defclass lazy-bidirectional-range (cl-ds:fundamental-bidirectional-range
                                    lazy-forward-range)
  ()
  (:metaclass funcallable-standard-class))

(defclass lazy-random-access-range (cl-ds:fundamental-random-access-range
                                    lazy-bidirectional-range)
  ()
  (:metaclass funcallable-standard-class))

(defmethod cl-ds:consume-front :before ((range lazy-range))
   (force-lazy-range range))

(defmethod cl-ds:peek-front :before ((range lazy-range))
  (force-lazy-range range))

(defmethod cl-ds:consume-back :before ((range lazy-range))
  (force-lazy-range range))

(defmethod cl-ds:peek-back :before ((range lazy-range))
  (force-lazy-range range))

(defmethod cl-ds:drop-front :before ((range lazy-range) count)
  (force-lazy-range range))

(defmethod cl-ds:drop-back :before ((range lazy-range) count)
  (force-lazy-range range))

(defmethod cl-ds:traverse :before ((range lazy-range) function)
  (force-lazy-range range))

(defmethod cl-ds:across :before ((range lazy-range) function)
  (force-lazy-range range))

(defmethod cl-ds:size :before ((range lazy-random-access-range))
  (force-lazy-range range))

(defmethod cl-ds:consume-front ((range lazy-forward-range))
  (cl-ds:consume-front (slot-value range '%range)))

(defmethod cl-ds:peek-front ((range lazy-forward-range))
  (cl-ds:peek-front (slot-value range '%range)))

(defmethod cl-ds:consume-back ((range lazy-bidirectional-range))
  (cl-ds:consume-back (slot-value range '%range)))

(defmethod cl-ds:peek-back ((range lazy-bidirectional-range))
  (cl-ds:peek-back (slot-value range '%range)))

(defmethod cl-ds:drop-front ((range lazy-forward-range) count)
  (cl-ds:drop-front (slot-value range '%range) count))

(defmethod cl-ds:drop-back ((range lazy-bidirectional-range) count)
  (cl-ds:drop-back (slot-value range '%range) count))

(defmethod cl-ds:traverse ((range lazy-forward-range) function)
  (cl-ds:traverse (slot-value range '%range) function))

(defmethod cl-ds:across ((range lazy-forward-range) function)
  (cl-ds:across (slot-value range '%range) function))

(defmethod cl-ds:size ((range lazy-random-access-range))
  (cl-ds:size (slot-value range '%range)))
