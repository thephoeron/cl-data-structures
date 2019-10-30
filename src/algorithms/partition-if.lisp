(in-package #:cl-data-structures.algorithms)


(defclass abstract-partition-if-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%test :initarg :test
          :reader read-test)))


(defclass forward-abstract-partition-if-proxy
    (abstract-partition-if-proxy
     fundamental-forward-range)
  ())


(defclass bidirectional-abstract-partition-if-proxy
    (forward-abstract-partition-if-proxy
     fundamental-bidirectional-range)
  ())


(defclass random-access-abstract-parition-if-proxy
    (bidirectional-abstract-partition-if-proxy
     fundamental-random-access-range)
  ())


(defclass callback-partition-if-proxy ()
  ((%callback :reader read-callback
              :initarg :callback)))


(defclass forward-callback-partition-if-proxy
    (callback-partition-if-proxy forward-abstract-partition-if-proxy)
  ())


(defclass bidirectional-callback-partition-if-proxy
    (callback-partition-if-proxy bidirectional-abstract-partition-if-proxy)
  ())


(defclass random-access-callback-parition-if-proxy
    (callback-partition-if-proxy random-access-abstract-parition-if-proxy)
  ())


(defclass partition-if-proxy ()
  ((%chunks :initform (vect)
            :reader read-chunks)))


(defclass forward-partition-if-proxy
    (partition-if-proxy forward-abstract-partition-if-proxy)
  ())


(defclass bidirectional-partition-if-proxy
    (partition-if-proxy bidirectional-abstract-partition-if-proxy)
  ())


(defclass random-access-parition-if-proxy
    (partition-if-proxy random-access-abstract-parition-if-proxy)
  ())


(defmethod clone ((range callback-partition-if-proxy))
  (make (type-of range)
        :callback (read-callback range)
        :original-range (~> range read-original-range clone)
        :key (read-key range)
        :test (read-test range)))


(defmethod clone ((range abstract-partition-if-proxy))
  (make (type-of range)
        :original-range (~> range read-original-range clone)
        :key (read-key range)
        :test (read-test range)))


(defclass abstract-partition-if-aggregator ()
  ((%outer-fn :initarg :outer-fn
              :reader read-outer-fn)
   (%test :initarg :test
          :reader read-test)
   (%partition-key :initarg :partition-key
                   :reader read-partition-key)
   (%key :initarg :key
         :reader read-key)))


(defclass callback-partition-if-aggregator (abstract-partition-if-aggregator
                                            cl-ds.alg.meta:fundamental-aggregator)
  ((%callback :reader read-callback
              :initarg :callback)
   (%current-state :accessor access-current-state
                   :initform nil)
   (%current-key :accessor access-current-key
                 :initform nil)
   (%initialized :accessor access-initialized
                 :type boolean
                 :initform nil)))


(defclass partition-if-aggregator (abstract-partition-if-aggregator
                                   cl-ds.alg.meta:fundamental-aggregator)
  ((%chunks :initform (vect)
            :initarg :chunks
            :type vector
            :reader read-chunks)
   (%current-index :initform 0
                   :accessor access-current-index)))


(defclass linear-partition-if-aggregator (partition-if-aggregator)
  ((%finished :initform nil
              :reader cl-ds.alg.meta:aggregator-finished-p
              :accessor access-finished)))


(defclass linear-callback-partition-if-aggregator (callback-partition-if-aggregator)
  ((%finished :initform nil
              :reader cl-ds.alg.meta:aggregator-finished-p
              :accessor access-finished)))


(defclass multi-partition-if-aggregator (partition-if-aggregator
                                         multi-aggregator)
  ((%stages :initarg :stages
            :accessor access-stages)))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator callback-partition-if-aggregator))
  nil)


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator callback-partition-if-aggregator))
  t)


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


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator linear-callback-partition-if-aggregator))
  (when (access-initialized aggregator)
    (~> aggregator access-current-state cl-ds.alg.meta:end-aggregation)
    (~>> aggregator access-current-state
         cl-ds.alg.meta:extract-result
         (funcall (read-callback aggregator))))
  (setf (access-finished aggregator) t))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator multi-partition-if-aggregator))
  (~> aggregator access-stages endp))


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator linear-partition-if-aggregator))
  t)


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator multi-partition-if-aggregator))
  (~> aggregator access-stages first
      (cl-ds.alg.meta:expects-content-with-stage-p aggregator)))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator partition-if-aggregator)
                                               element)
  (bind (((:slots %chunks %partition-key %test %outer-fn %current-index) aggregator)
         (key (funcall %partition-key element))
         (length (length %chunks))
         (empty (zerop length)))
    (if empty
        (let ((sub (funcall %outer-fn)))
          (begin-aggregation sub)
          (vector-push-extend (list* key sub)
                              %chunks))
        (iterate
          (for (prev . chunk) = (aref %chunks %current-index))
          (until (funcall %test prev key))
          (incf %current-index)
          (when (>= %current-index (length %chunks))
            (let ((sub (funcall %outer-fn)))
              (begin-aggregation sub)
              (vector-push-extend (list* key sub)
                                  %chunks)
              (leave)))))
    (setf (car (aref %chunks %current-index)) key)
    (~> %chunks (aref %current-index) cdr (pass-to-aggregation element))))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator callback-partition-if-aggregator)
                                               element)
  (bind (((:slots %initialized %partition-key %test %outer-fn %callback %current-state %current-key)
          aggregator)
         (key (funcall %partition-key element)))
    (cond ((not %initialized)
           (setf %current-key key
                 %initialized t
                 %current-state (funcall %outer-fn))
           (begin-aggregation %current-state)
           (pass-to-aggregation %current-state element))
          ((funcall %test %current-key key)
           (setf %current-key key)
           (pass-to-aggregation %current-state element))
          (t
           (cl-ds.alg.meta:end-aggregation %current-state)
           (~>> %current-state
                cl-ds.alg.meta:extract-result
                (funcall %callback))
           (setf %current-key key
                 %current-state (funcall %outer-fn))
             (begin-aggregation %current-state)
             (pass-to-aggregation %current-state element)))))


(defmethod cl-ds.alg.meta:extract-result ((aggregator callback-partition-if-aggregator))
  nil)


(defmethod cl-ds.alg.meta:extract-result ((aggregator partition-if-aggregator))
  (bind (((:slots %chunks) aggregator))
    (~> (map 'vector
             (compose #'cl-ds.alg.meta:extract-result #'cdr)
             %chunks)
        cl-ds:whole-range)))


(defmethod proxy-range-aggregator-outer-fn ((range callback-partition-if-proxy)
                                            key
                                            function
                                            outer-fn
                                            arguments)
  (let ((outer-fn (call-next-method)))
    (if (typep function 'cl-ds.alg.meta:multi-aggregation-function)
        (error 'cl-ds:operation-not-allowed
               :format-control "Can't use callback-partition-if with multi-aggregation-function.")
        (lambda ()
          (make 'linear-callback-partition-if-aggregator
                :outer-fn outer-fn
                :key key
                :callback (read-callback range)
                :test (read-test range)
                :partition-key (read-key range))))))


(defmethod proxy-range-aggregator-outer-fn ((range partition-if-proxy)
                                            key
                                            function
                                            outer-fn
                                            arguments)
  (let ((outer-fn (call-next-method)))
    (if (typep function 'cl-ds.alg.meta:multi-aggregation-function)
        (lambda ()
          (make 'multi-partition-if-aggregator
                :outer-fn outer-fn
                :test (read-test range)
                :key key
                :partition-key (read-key range)
                :stages (apply #'cl-ds.alg.meta:multi-aggregation-stages
                               function
                               arguments)))
        (lambda ()
          (make 'linear-partition-if-aggregator
                :outer-fn outer-fn
                :key key
                :test (read-test range)
                :partition-key (read-key range))))))


(defclass partition-if-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass callback-partition-if-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric partition-if-with-callback (range test callback &key key)
  (:generic-function-class callback-partition-if-function)
  (:method (range test callback &key (key #'identity))
    (ensure-functionf test key callback)
    (apply-range-function range #'partition-if-with-callback
                          :callback callback
                          :key key
                          :test test)))


(defgeneric partition-if (range test &key key)
  (:generic-function-class partition-if-function)
  (:method (range test &key (key #'identity))
    (ensure-functionf test key)
    (apply-range-function range #'partition-if
                          :key key
                          :test test)))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn partition-if-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'forward-partition-if-proxy
              :key key
              :test test))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn partition-if-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'bidirectional-partition-if-proxy
              :key key
              :test test))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn partition-if-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'random-access-parition-if-proxy
              :key key
              :test test))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn callback-partition-if-function)
                        &rest all &key test callback key)
  (declare (ignore all))
  (make-proxy range 'forward-callback-partition-if-proxy
              :key key
              :callback callback
              :test test))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn callback-partition-if-function)
                        &rest all &key test callback key)
  (declare (ignore all))
  (make-proxy range 'bidirectional-callback-partition-if-proxy
              :key key
              :callback callback
              :test test))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn callback-partition-if-function)
                        &rest all &key test callback key)
  (declare (ignore all))
  (make-proxy range 'random-access-callback-parition-if-proxy
              :key key
              :callback callback
              :test test))
