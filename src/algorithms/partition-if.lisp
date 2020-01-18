(in-package #:cl-data-structures.algorithms)


(defclass abstract-partition-if-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%on-first :initarg :on-first
              :reader read-on-first)
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
        :on-first (read-on-first range)
        :original-range (~> range read-original-range clone)
        :key (read-key range)
        :test (read-test range)))


(defmethod clone ((range abstract-partition-if-proxy))
  (make (type-of range)
        :original-range (~> range read-original-range clone)
        :on-first (read-on-first range)
        :key (read-key range)
        :test (read-test range)))


(defclass abstract-partition-if-aggregator ()
  ((%outer-fn :initarg :outer-fn
              :reader read-outer-fn)
   (%on-first :initarg :on-first
              :reader read-on-first)
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
  ())


(defclass linear-callback-partition-if-aggregator (callback-partition-if-aggregator)
  ())


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator partition-if-aggregator)
                                               element)
  (bind (((:slots %chunks %partition-key %test %outer-fn %current-index
                  %on-first)
          aggregator)
         (key (funcall %partition-key element))
         (length (length %chunks))
         (empty (zerop length)))
    (if empty
        (let ((sub (funcall %outer-fn)))
          (vector-push-extend (list* key sub)
                              %chunks))
        (iterate
          (for (prev . chunk) = (aref %chunks %current-index))
          (until (funcall %test prev key))
          (incf %current-index)
          (when (>= %current-index (length %chunks))
            (let ((sub (funcall %outer-fn)))
              (vector-push-extend (list* key sub)
                                  %chunks)
              (leave)))))
    (unless %on-first
      (setf (car (aref %chunks %current-index)) key))
    (~> %chunks (aref %current-index) cdr (pass-to-aggregation element))))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator callback-partition-if-aggregator)
                                               element)
  (bind (((:slots %initialized %partition-key %test %outer-fn %callback
                  %current-state %current-key
                  %on-first)
          aggregator)
         (key (funcall %partition-key element)))
    (cond ((not %initialized)
           (setf %current-key key
                 %initialized t
                 %current-state (funcall %outer-fn))
           (pass-to-aggregation %current-state element))
          ((funcall %test %current-key key)
           (unless %on-first
             (setf %current-key key))
           (pass-to-aggregation %current-state element))
          (t
           (~>> %current-state
                cl-ds.alg.meta:extract-result
                (funcall %callback))
           (setf %current-key key
                 %current-state (funcall %outer-fn))
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
    (lambda ()
      (make 'linear-callback-partition-if-aggregator
            :outer-fn outer-fn
            :key key
            :on-first (read-on-first range)
            :callback (read-callback range)
            :test (read-test range)
            :partition-key (read-key range)))))


(defmethod proxy-range-aggregator-outer-fn ((range partition-if-proxy)
                                            key
                                            function
                                            outer-fn
                                            arguments)
  (let ((outer-fn (call-next-method)))
    (lambda ()
      (make 'linear-partition-if-aggregator
            :outer-fn outer-fn
            :key key
            :on-first (read-on-first range)
            :test (read-test range)
            :partition-key (read-key range)))))


(defclass partition-if-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass callback-partition-if-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric partition-if-with-callback (range test callback &key key on-first)
  (:generic-function-class callback-partition-if-function)
  (:method (range test callback &key (key #'identity) (on-first nil))
    (ensure-functionf test key callback)
    (apply-range-function range #'partition-if-with-callback
                          :callback callback
                          :on-first on-first
                          :key key
                          :test test)))


(defgeneric partition-if (range test &key key on-first)
  (:generic-function-class partition-if-function)
  (:method (range test &key (key #'identity) (on-first nil))
    (ensure-functionf test key)
    (apply-range-function range #'partition-if
                          :key key
                          :on-first on-first
                          :test test)))


(defmethod apply-layer ((range traversable)
                        (fn partition-if-function)
                        &rest all &key test key on-first)
  (declare (ignore all))
  (make-proxy range 'forward-partition-if-proxy
              :key key
              :on-first on-first
              :test test))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn partition-if-function)
                        &rest all &key test key on-first)
  (declare (ignore all))
  (make-proxy range 'bidirectional-partition-if-proxy
              :key key
              :on-first on-first
              :test test))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn partition-if-function)
                        &rest all &key test key on-first)
  (declare (ignore all))
  (make-proxy range 'random-access-parition-if-proxy
              :key key
              :on-first on-first
              :test test))


(defmethod apply-layer ((range traversable)
                        (fn callback-partition-if-function)
                        &rest all &key test callback key on-first)
  (declare (ignore all))
  (make-proxy range 'forward-callback-partition-if-proxy
              :key key
              :on-first on-first
              :callback callback
              :test test))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn callback-partition-if-function)
                        &rest all &key test callback key on-first)
  (declare (ignore all))
  (make-proxy range 'bidirectional-callback-partition-if-proxy
              :key key
              :on-first on-first
              :callback callback
              :test test))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn callback-partition-if-function)
                        &rest all &key test callback key on-first)
  (declare (ignore all))
  (make-proxy range 'random-access-callback-parition-if-proxy
              :key key
              :on-first on-first
              :callback callback
              :test test))
