(in-package #:cl-data-structures.math)


(defclass bootstrap-proxy (cl-ds.alg:proxy-range)
  ((%sample-size :initarg :sample-size
                 :reader read-sample-size)
   (%key :initarg :key
         :reader read-key)
   (%compare :initarg :compare
             :reader read-compare)
   (%confidence :initarg :confidence
                :reader read-confidence)
   (%parallel :initarg :parallel
              :reader read-parallel)
   (%context-function :initarg :context-function
                      :reader read-context-function)
   (%samples-count :initarg :samples-count
                   :reader read-samples-count)))


(defmethod cl-ds.utils:cloning-information append
    ((range bootstrap-proxy))
  '((:sample-size read-sample-size)
    (:key read-key)
    (:confidence read-confidence)
    (:compare read-compare)
    (:parallel read-parallel)
    (:context-function read-context-function)
    (:samples-count read-samples-count)))


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
   (%compare :initarg :compare
             :reader read-compare)
   (%confidence :initarg :confidence
                :reader read-confidence)
   (%whole-content :initform (vect)
                   :reader read-whole-content)
   (%sample-size :initarg :sample-size
                 :reader read-sample-size)
   (%parallel :initarg :parallel
              :reader read-parallel)
   (%function :initarg :function
              :accessor access-function)
   (%context-function :initarg :context-function
                      :reader read-context-function)
   (%final-result :initform nil
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
            :key (cl-ds.alg.meta:read-key range)
            :compare (read-compare range)
            :parallel (read-parallel range)
            :context-function (read-context-function range)
            :confidence (read-confidence range)
            :sample-size (read-sample-size range)
            :samples-count (read-samples-count range)))))


(defclass bootstrap-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric bootstrap (range sample-size samples-count
                       &key confidence key compare parallel context-function)
  (:generic-function-class bootstrap-function)
  (:method (range sample-size samples-count
            &key
              (confidence 0.95) (key #'identity) (compare #'<)
              (parallel t) (context-function #'identity))
    (check-type samples-count integer)
    (check-type sample-size integer)
    (check-type confidence float)
    (cl-ds:check-argument-bounds confidence (< 0.0 confidence 1.0))
    (cl-ds:check-argument-bounds samples-count (< 8 samples-count))
    (cl-ds:check-argument-bounds samples-count (< 8 sample-size))
    (cl-ds.alg.meta:apply-range-function range #'bootstrap
                                         :confidence confidence
                                         :key key
                                         :compare compare
                                         :parallel parallel
                                         :sample-size sample-size
                                         :context-function context-function
                                         :samples-count samples-count)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn bootstrap-function)
                                       &rest all
                                       &key
                                         confidence sample-size samples-count
                                         context-function key compare parallel)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'forward-bootstrap-proxy
                        :sample-size sample-size
                        :confidence confidence
                        :key key
                        :compare compare
                        :parallel parallel
                        :context-function context-function
                        :samples-count samples-count))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn bootstrap-function)
                                       &rest all
                                       &key
                                         confidence sample-size parallel
                                         context-function samples-count key compare)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'bidirectional-bootstrap-proxy
                        :sample-size sample-size
                        :confidence confidence
                        :key key
                        :compare compare
                        :parallel parallel
                        :context-function context-function
                        :samples-count samples-count))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn bootstrap-function)
                                       &rest all
                                       &key
                                         confidence sample-size parallel
                                         context-function samples-count key compare)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'random-access-bootstrap-proxy
                        :sample-size sample-size
                        :confidence confidence
                        :key key
                        :compare compare
                        :context-function context-function
                        :parallel parallel
                        :samples-count samples-count))


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator bootstrap-aggregator))
  (not (access-finished aggregator)))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator bootstrap-aggregator) element)
  (vector-push-extend element (read-whole-content aggregator)))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator bootstrap-aggregator))
  (access-finished aggregator))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator bootstrap-aggregator))
  nil)


(defun aggregate-sample (aggregator sample function)
  (cl-ds.alg.meta:apply-aggregation-function-with-aggregator
   aggregator
   sample
   function))


(defun bootstrap-sample (vector size)
  (lret ((result (make-array size)))
    (iterate
      (for i from 0 below size)
      (setf (aref result i) (aref vector (random (length vector)))))))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator bootstrap-aggregator))
  (setf (access-finished aggregator) t)
  (let* ((samples-vector (make-array (read-samples-count aggregator)))
         (sample-size (read-sample-size aggregator))
         (whole-content (read-whole-content aggregator))
         (samples-count (read-samples-count aggregator))
         (outer-fn (read-outer-fn aggregator))
         (function (access-function aggregator))
         (confidence (read-confidence aggregator))
         (offset (~> confidence (/ 2) (* samples-count)))
         (half (/ samples-count 2))
         (parallel (read-parallel aggregator))
         (lower-percentail (floor (- half offset)))
         (higher-percentail (ceiling (+ half offset))))
    (funcall (if parallel #'lparallel:pmap-into #'map-into)
             samples-vector
             (funcall (read-context-function aggregator)
                      (lambda ()
                        (let* ((sample (bootstrap-sample whole-content sample-size))
                               (fresh-aggregator (funcall outer-fn)))
                          (aggregate-sample fresh-aggregator sample function)))))
    (setf samples-vector
          (lparallel:psort samples-vector
                           (read-compare aggregator)
                           :key (cl-ds.alg.meta:read-key aggregator))

          (access-final-result aggregator)
          (list* (aref samples-vector lower-percentail)
                 (aref samples-vector higher-percentail)))))
