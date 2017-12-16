(in-package #:cl-data-structures.common)


(defclass lazy-range ()
  ((%container :initarg :container
               :reader read-container)
   (%range :initarg :range
           :reader read-range
           :type (or (-> () cl-ds:fundamental-range)
                     cl-ds:fundamental-range))))


(defun force-lazy-range (range)
  (declare (type lazy-range range))
  (with-slots (%range %container) range
    (force-version %container)
    (when (functionp %range)
      (setf %range (funcall %range)))))


(defmethod cl-ds:empty-clone-of-inner-container ((range lazy-range))
  (cl-ds:empty-clone (read-container range)))


(defmacro make-lazy-range (class container range)
  `(make-instance ',class
                  :container ,container
                  :range (lambda () ,range)))


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


(defclass lazy-forward-range (cl-ds:fundamental-forward-range
                              lazy-range)
  ())


(defclass lazy-bidirectional-range (cl-ds:fundamental-bidirectional-range
                                    lazy-forward-range)
  ())


(defclass lazy-random-access-range (cl-ds:fundamental-random-access-range
                                    lazy-bidirectional-range)
  ())


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


(defmethod cl-ds:morep ((range lazy-range))
  (cl-ds:morep (slot-value range '%range)))


(defmethod cl-ds:drop-back ((range lazy-bidirectional-range) count)
  (cl-ds:drop-back (slot-value range '%range) count))
