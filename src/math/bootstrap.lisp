(cl:in-package #:cl-data-structures.math)


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
                                         (list range sample-size samples-count
                                               :confidence confidence
                                               :key key
                                               :compare compare
                                               :parallel parallel
                                               :context-function context-function))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn bootstrap-function)
                                       all)
  (flet ((destruct (range sample-size samples-count
                    &key confidence key compare parallel context-function)
           (cl-ds.alg:make-proxy range 'forward-bootstrap-proxy
                                 :sample-size sample-size
                                 :confidence confidence
                                 :key key
                                 :compare compare
                                 :parallel parallel
                                 :context-function context-function
                                 :samples-count samples-count)))
    (apply #'destruct all)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn bootstrap-function)
                                       all)
  (flet ((destruct (range sample-size samples-count
                    &key confidence key compare parallel context-function)
           (cl-ds.alg:make-proxy range 'bidirectional-bootstrap-proxy
                                 :sample-size sample-size
                                 :confidence confidence
                                 :key key
                                 :compare compare
                                 :parallel parallel
                                 :context-function context-function
                                 :samples-count samples-count)))
    (apply #'destruct all)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn bootstrap-function)
                                       all)
  (flet ((destruct (range sample-size samples-count
                    &key confidence key compare parallel context-function)
           (cl-ds.alg:make-proxy range 'random-access-bootstrap-proxy
                                 :sample-size sample-size
                                 :confidence confidence
                                 :key key
                                 :compare compare
                                 :parallel parallel
                                 :context-function context-function
                                 :samples-count samples-count)))
    (apply #'destruct all)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range bootstrap-proxy)
                                                  outer-constructor
                                                  (function cl-ds.alg.meta:aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((outer-fn (call-next-method))
         (sample-size (read-sample-size range))
         (key (ensure-function (read-key range)))
         (compare (ensure-function (read-compare range)))
         (samples-count (read-samples-count range))
         (confidence (read-confidence range))
         (offset (~> confidence (/ 2) (* samples-count)))
         (half (/ samples-count 2))
         (lower-percentail (floor (- half offset)))
         (higher-percentail (ceiling (+ half offset)))
         (parallel (read-parallel range))
         (context-function (ensure-function (read-context-function range))))
    (declare (type fixnum lower-percentail higher-percentail))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.alg.meta:let-aggregator
         ((data (vect)))

         ((element) (vector-push-extend element (funcall key data)))

         ((let ((samples (make-array samples-count))
                (length (length data)))
            (declare (type simple-array samples))
            (funcall (if parallel #'lparallel:pmap-into #'map-into)
                     samples
                     (lambda ()
                       (funcall context-function
                                (lambda (&aux (aggregator (cl-ds.alg.meta:call-constructor outer-fn)))
                                  (handler-case
                                      (iterate
                                        (declare (type fixnum i))
                                        (for i from 0 below sample-size)
                                        (for random = (random length))
                                        (cl-ds.alg.meta:pass-to-aggregation aggregator (aref data random))
                                        (finally (return (cl-ds.alg.meta:extract-result aggregator))))
                                    (error (e)
                                      (cl-ds.alg.meta:cleanup aggregator)
                                      (error e)))))))
            (setf samples (sort samples compare))
            (list* (aref samples lower-percentail)
                   (aref samples higher-percentail)))))
     function
     arguments)))
