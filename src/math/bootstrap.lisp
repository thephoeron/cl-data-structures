(in-package #:cl-data-structures.math)


(defclass bootstrap-proxy (cl-ds.alg:proxy-range)
  ((%sample-size :initarg :sample-size
                 :reader read-sample-size)
   (%samples-count :initarg :samples-count
                   :reader read-samples-count)))


(defclass forward-bootstrap-proxy (bootstrap-proxy
                                   cl-ds:fundamental-forward-range)
  ())


(defclass bidirectional-bootstrap-proxy (bootstrap-proxy
                                         cl-ds:fundamental-bidirectional-range)
  ())


(defclass random-access-bootstrap-proxy (bootstrap-proxy
                                         cl-ds:fundamental-random-access-range)
  ())


(defclass bootstrap-aggregator (cl-ds.alg.meta:fundamental-aggregator)
  ((%outer-fn :initarg :outer-fn
              :reader read-outer-fn)
   (%whole-content :initform (vect)
                   :reader read-whole-content)
   (%sample-size :initarg :sample-size
                 :reader read-sample-size)
   (%function :initarg :function
              :accessor access-function)
   (final-result :initform nil
                 :accessor access-final-result
                 :reader cl-ds.alg.meta:extract-result)
   (%samples-count :initarg :samples-count
                   :reader read-samples-count)
   (%finished :initform nil
              :reader cl-ds.alg.meta:aggregator-finished-p
              :accessor access-finished)))


(defmethod cl-ds.alg:proxy-range-aggregator-outer-fn
    ((range bootstrap-proxy)
     key
     function
     outer-fn
     arguments)
  (bind ((outer-fn (call-next-method)))
    (lambda ()
      (make 'bootstrap-aggregator
            :outer-fn outer-fn
            :function function
            :sample-size (read-sample-size range)
            :samples-count (read-samples-count range)))))


(defclass bootstrap-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric bootstrap (range sample-size samples-count)
  (:generic-function-class bootstrap-function)
  (:method (range sample-size samples-count)
    (cl-ds.alg.meta:apply-range-function range #'bootstrap
                                         :sample-size sample-size
                                         :samples-count samples-count)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn bootstrap-function)
                                       &rest all &key sample-size samples-count)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'forward-bootstrap-proxy
                        :sample-size sample-size
                        :samples-count samples-count))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn bootstrap-function)
                                       &rest all &key sample-size samples-count)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'bidirectional-bootstrap-proxy
                        :sample-size sample-size
                        :samples-count samples-count))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn bootstrap-function)
                                       &rest all &key sample-size samples-count)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'random-access-bootstrap-proxy
                        :sample-size sample-size
                        :samples-count samples-count))


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator bootstrap-aggregator))
  (not (access-finished aggregator)))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator bootstrap-aggregator) element)
  (vector-push-extend element (read-whole-content aggregator)))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator bootstrap-aggregator))
  (access-finished aggregator))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator bootstrap-aggregator))
  nil)


(defun bootstrap-sample (aggregator sample function)
  (cl-ds.alg.meta:apply-aggregation-function-with-aggregator
   aggregator
   sample
   function))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator bootstrap-aggregator))
  (setf (access-finished aggregator) t)
  (iterate
    (with samples-vector = (make-array (read-samples-count aggregator)
                                       :element-type 'real))
    (for i from 0 below (read-samples-count aggregator))
    (for sample = (cl-ds.utils:draw-sample-vector (read-whole-content aggregator)
                                                  (read-samples-count aggregator)))
    (for fresh-aggregator = (funcall (read-outer-fn aggregator)))
    (for result = (bootstrap-sample fresh-aggregator
                                    sample
                                    (access-function aggregator)))
    (setf (aref samples-vector i) result)
    (finally (setf (access-final-result aggregator) (statistical-summary samples-vector)))))
