(in-package #:cl-data-structures.statistics)


(defclass variance-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric variance (range &key key biased)
  (:generic-function-class entropy-function)
  (:method (range &key (key #'identity) (biased t))
    (cl-ds.alg:apply-aggregation-function range
                                          #'variance
                                          :key key
                                          :range range
                                          :biased biased)))


(defstruct variance-state range (count 0 :type fixnum) sum biased key)


(defmethod cl-ds.alg:make-state ((function variance-function)
                                 &rest all &key biased range key
                                 &allow-other-keys)
  (declare (ignore all))
  (make-variance-state :range range :biased biased :key key))


(defmethod cl-ds.alg:state-result ((function variance-function) state)
  (with-accessors ((range variance-state-range)
                   (count variance-state-count)
                   (sum variance-state-sum)
                   (key variance-state-key)
                   (biased variance-state-biased)) state
    (let ((total 0)
          (avg (/ sum count)))
      (cl-ds:traverse (lambda (x) (incf total (expt (- (funcall key x) avg) 2)))
                      range)
      (/ total (- count (if biased 0 1))))))


(defmethod cl-ds.alg:aggregate ((function variance-function) state element)
  (with-accessors ((range variance-state-range)
                   (count variance-state-count)
                   (sum variance-state-sum)
                   (biased variance-state-biased)) state
    (incf count)
    (incf sum element)))
