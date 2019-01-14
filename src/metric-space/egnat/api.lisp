(in-package #:cl-data-structures.metric-space.egnat)


(defclass egnat-metric-set (cl-ds.ms:metric-space-set
                            cl-ds.common.egnat:fundamental-egnat-container)
  ())


(defclass mutable-egnat-metric-set (cl-ds.ms:mutable-metric-space-set
                                    cl-ds.common.egnat:mutable-egnat-container)
  ())


(defmethod cl-ds.common.egnat:same ((container egnat-metric-set)
                                    bucket item)
  (cl-ds.ms:same container bucket item))


(defmethod cl-ds.common.egnat:distance ((container egnat-metric-set)
                                        bucket item)
  (cl-ds.ms:distance container bucket item))


(defun make-mutable-egnat-metric-set (same-function distance-function distance-type
                                      &key (branching-factor 20) (node-size 50)
                                      &allow-other-keys)
  (ensure-functionf same-function distance-function)
  (make 'egnat-metric-set
        :metric-fn distance-function
        :same-fn same-function
        :metric-type distance-type
        :branching-factor branching-factor
        :content-count-in-node node-size))


(defmethod cl-ds:make-from-traversable ((class (eql 'mutable-egnat-metric-set))
                                        (sequence vector)
                                        &rest arguments)
  (bind ((container (apply #'make-mutable-egnat-metric-set arguments))
         (root (cl-ds.common.egnat:make-egnat-tree container
                                                   #'cl-ds:put!
                                                   (second (member :parallel arguments))
                                                   sequence)))
    (setf (cl-ds.common.egnat:access-root container) root
          (cl-ds.common.egnat:access-size container) (cl-ds:size sequence))
    container))


(defmethod cl-ds:make-from-traversable ((class (eql 'mutable-egnat-metric-set))
                                        (sequence cl-ds:fundamental-random-access-range)
                                        &rest arguments)
  (bind ((container (apply #'make-mutable-egnat-metric-set arguments))
         (root (cl-ds.common.egnat:make-egnat-tree container
                                                   #'cl-ds:put!
                                                   (second (member :parallel arguments))
                                                   sequence)))
    (setf (cl-ds.common.egnat:access-root container) root
          (cl-ds.common.egnat:access-size container) (cl-ds:size sequence))
    container))


(defmethod cl-ds:make-from-traversable ((class (eql 'mutable-egnat-metric-set))
                                        (sequence cl-ds:fundamental-forward-range)
                                        &rest arguments)
  (let ((vector (vect)))
    (cl-ds:across sequence (rcurry #'vector-push-extend vector))
    (apply #'cl-ds:make-from-traversable class vector arguments)))
