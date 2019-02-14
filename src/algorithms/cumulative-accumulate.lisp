(in-package #:cl-data-structures.algorithms)


(defclass cumulative-state ()
  ((%initial-state :initarg :state
                   :reader read-initial-state)
   (%state :initarg :state
           :accessor access-state)
   (%result :initarg :result
            :reader read-result)
   (%function :initarg :function
              :reader read-function)
   (%key :initarg :key
         :reader read-key)))


(defclass cumulative-accumulate-range (cl-ds.alg:proxy-range
                                       cl-ds:fundamental-forward-range
                                       cumulative-state)
  ())


(defmethod cl-ds:clone ((range cumulative-accumulate-range))
  (apply #'make (type-of range)
         :original-range (read-original-range range)
         :result (read-result range)
         :function (read-function range)
         :key (read-key range)
         (if (slot-boundp range '%state)
             (list :state (access-state range))
             nil)))


(defmethod cl-ds:reset! ((range cumulative-accumulate-range))
  (call-next-method)
  (if (slot-boundp range '%initial-state)
      (setf (access-state range) (read-initial-state range))
      (slot-makunbound range '%state))
  range)


(defmethod cl-ds:peek-front ((range cumulative-accumulate-range))
  (bind (((:values v more) (call-next-method)))
    (if (no more)
        (values v nil)
        (let* ((key (read-key range))
               (function (read-function range))
               (result (read-result range)))
          (values
           (if (slot-boundp range '%state)
               (~>> (funcall key v)
                    (funcall function (access-state range))
                    (funcall result))
               v)
           t)))))


(defun consume-impl (range v)
  (let* ((key (read-key range))
         (function (read-function range))
         (result (read-result range)))
    (values
     (if (slot-boundp range '%state)
         (let* ((state (access-state range))
                (next-state
                  (~>> (funcall key v)
                       (funcall function state))))
           (setf (access-state range) next-state)
           (funcall result next-state))
         v)
     t)))


(defmethod cl-ds:consume-front ((range cumulative-accumulate-range))
  (bind (((:values v more) (call-next-method)))
    (if (no more)
        (values v nil)
        (consume-impl range v))))


(defmethod cl-ds:traverse ((range cumulative-accumulate-range) function)
  (call-next-method range
                    (lambda (elt)
                      (funcall function
                               (consume-impl range elt)))))


(defmethod cl-ds:across ((range cumulative-accumulate-range) function)
  (let* ((state (apply #'make 'cumulative-state
                       :original-range (read-original-range range)
                       :result (read-result range)
                       :function (read-function range)
                       :key (read-key range)
                       (if (slot-boundp range '%state)
                           (list :state (access-state range))
                           nil))))
    (call-next-method range
                      (lambda (elt)
                        (funcall function
                                 (consume-impl state elt))))))


(defclass cumulative-accumulate-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric cumulative-accumulate (range function
                                   &key key result initial-value)
  (:generic-function-class cumulative-accumulate-function)
  (:method (range function
            &key
              (key #'identity) (result #'identity)
              (initial-value nil initial-value-bound))
    (ensure-functionf key result function)
    (cl-ds.alg.meta:apply-range-function
     range
     #'cumulative-accumulate
     :function function
     :result result
     :initial-value initial-value
     :initial-value-bound initial-value-bound
     :key key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn cumulative-accumulate-function)
                                       &rest all
                                       &key
                                         key function result
                                         initial-value
                                         initial-value-bound)
  (declare (ignore all))
  (apply #'cl-ds.alg:make-proxy
         range 'cumulative-accumulate-range
         :key key
         :function function
         :result result
         (if initial-value-bound
             (list :state initial-value)
             nil)))
