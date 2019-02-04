(in-package #:cl-data-structures.algorithms)


(defclass partition-if-proxy (proxy-range)
  ((%chunks :initform (vect)
            :reader read-chunks)
   (%test :initarg :test
          :reader read-test)))


(defclass forward-partition-if-proxy
    (partition-if-proxy fundamental-forward-range)
  ())


(defclass bidirectional-partition-if-proxy
    (forward-partition-if-proxy fundamental-bidirectional-range)
  ())


(defclass random-access-parition-if-proxy
    (bidirectional-partition-if-proxy fundamental-random-access-range)
  ())


(defmethod clone ((range partition-if-proxy))
  (make-instance (type-of range)
                 :original-range (~> range read-original-range clone)
                 :test (read-test range)))


(defclass partition-if-aggregator (cl-ds.alg.meta:fundamental-aggregator)
  ((%chunks :initform (vect)
            :initarg :chunks
            :type vector
            :reader read-chunks)
   (%current-index :initform 0
                   :accessor access-current-index)
   (%outer-fn :initarg :outer-fn
              :reader read-outer-fn)
   (%test :initarg :test
          :reader read-test)
   (%key :initarg :key
         :reader read-key)))


(defclass linear-partition-if-aggregator (partition-if-aggregator)
  ((%finished :initform nil
              :reader cl-ds.alg.meta:aggregator-finished-p
              :accessor access-finished)))


(defclass multi-partition-if-aggregator (partition-if-aggregator
                                         multi-aggregator)
  ((%stages :initarg :stages
            :accessor access-stages)))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator partition-if-aggregator))
  (setf (access-current-index aggregator) 0)
  (iterate
    (for (prev . chunk) in-vector (read-chunks aggregator))
    (begin-aggregation chunk)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator partition-if-aggregator))
  (iterate
    (for (prev . chunk) in-vector (read-chunks aggregator))
    (end-aggregation chunk)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator multi-partition-if-aggregator))
  (assert (access-stages aggregator))
  (call-next-method)
  (setf (access-stages aggregator) (rest (access-stages aggregator))))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator linear-partition-if-aggregator))
  (setf (access-finished aggregator) t)
  (call-next-method))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator multi-partition-if-aggregator))
  (~> aggregator access-stages endp))


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator linear-partition-if-aggregator))
  t)


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator multi-partition-if-aggregator))
  (~> aggregator access-stages first
      (cl-ds.alg.meta:expects-content-with-stage-p aggregator)))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator partition-if-aggregator)
                                               element)
  (bind (((:slots %chunks %test %outer-fn %current-index) aggregator)
         (length (length %chunks))
         (empty (zerop length)))
    (if empty
        (let ((sub (funcall %outer-fn)))
          (begin-aggregation sub)
          (vector-push-extend (list* element sub)
                              %chunks))
        (iterate
          (for (prev . chunk) = (aref %chunks %current-index))
          (until (funcall %test prev element))
          (incf %current-index)
          (when (>= %current-index (length %chunks))
            (let ((sub (funcall %outer-fn)))
              (begin-aggregation sub)
              (vector-push-extend (list* element sub)
                                  %chunks)
              (leave)))))
    (setf (car (aref %chunks %current-index)) element)
    (~> %chunks (aref %current-index) cdr (pass-to-aggregation element))))


(defmethod cl-ds.alg.meta:extract-result ((aggregator partition-if-aggregator))
  (bind (((:slots %chunks) aggregator))
    (~> (map 'vector
             (compose #'cl-ds.alg.meta:extract-result #'cdr)
             %chunks)
        cl-ds:whole-range)))


(defmethod proxy-range-aggregator-outer-fn ((range partition-if-proxy)
                                            key
                                            function
                                            outer-fn
                                            arguments)
  (bind ((outer-fn (call-next-method)))
    (if (typep function 'cl-ds.alg.meta:multi-aggregation-function)
        (lambda ()
          (make 'multi-partition-if-aggregator
                :outer-fn outer-fn
                :test (read-test range)
                :key key
                :stages (apply #'cl-ds.alg.meta:multi-aggregation-stages
                               function
                               arguments)))
        (lambda ()
          (make 'linear-partition-if-aggregator
                :outer-fn outer-fn
                :test (read-test range)
                :key key)))))


(defclass partition-if-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric partition-if (range test)
  (:generic-function-class partition-if-function)
  (:method (range test)
    (ensure-functionf test)
    (apply-range-function range #'partition-if
                          :test test)))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn partition-if-function)
                        &rest all &key test)
  (declare (ignore all))
  (make-proxy range 'forward-partition-if-proxy
              :test test))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn partition-if-function)
                        &rest all &key test)
  (declare (ignore all))
  (make-proxy range 'bidirectional-partition-if-proxy
              :test test))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn partition-if-function)
                        &rest all &key test)
  (declare (ignore all))
  (make-proxy range 'random-access-parition-if-proxy
              :test test))
