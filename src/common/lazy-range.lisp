(in-package #:cl-data-structures.common)


(defclass lazy-range ()
  ((%container :initarg :container
               :reader read-container)
   (%range :initarg :range
           :reader read-range)))


(defun force-lazy-range (range)
  (declare (type lazy-range range))
  (force-version (slot-value range '%range)))


(defmethod cl-ds:consume-front :before ((range lazy-range))
   (force-lazy-range range))


(defmethod cl-ds:peek-front :before ((range lazy-range))
  (force-lazy-range range))


(defmethod cl-ds:consume-back :before ((range lazy-range))
  (force-lazy-range range))


(defmethod cl-ds:peek-back :before ((range lazy-range))
  (force-lazy-range range))


;; (defmethod cl-ds:consume-some :before ((range lazy-range) count)
;;   (force-lazy-range range))


(defmethod cl-ds:drop-front :before ((range lazy-range) count)
  (force-lazy-range range))


(defmethod cl-ds:drop-back :before ((range lazy-range) count)
  (force-lazy-range range))


(defmethod cl-ds:morep :before ((range lazy-range))
  (force-lazy-ragne range))

