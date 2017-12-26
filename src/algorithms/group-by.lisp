(in-package #:cl-data-structures.algorithms)


(defclass group-by-proxy (proxy-range)
  ((%groups :initarg :groups
            :type hash-table
            :reader read-groups)
   (%key :initarg key
         :type hash-table
         :reader read-key)))


(defclass forward-group-by-proxy (group-by-proxy fundamental-forward-range)
  ())


(defclass bidirectional-group-by-proxy (forward-group-by-proxy fundamental-bidirectional-range)
  ())


(defclass random-access-group-by-proxy (bidirectional-group-by-proxy fundamental-random-access-range)
  ())


(defmethod clone ((range group-by-proxy))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :groups (copy-hash-table (read-groups range))
                 :key (read-key range)))


(defmethod initialize-instance :before ((instance group-by-proxy) &key test key &allow-other-keys)
  (setf (slot-value instance '%groups) (make-hash-table :test test)
        (slot-value instance '%key) key))


(defclass group-by-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric group-by (range &key test key)
  (:generic-function-class group-by-function)
  (:method (range &key (test 'eql) (key #'identity))
    (apply-range-function range #'group-by :test test :key key)))


(defclass group-by-states-collector (proxy-range)
  ((%label :initarg :label
           :initform nil
           :accessor access-label)
   (%accumulated-args :initarg :accumulated-args
                      :reader read-accumulated-args)
   (%groups :initarg :groups
            :reader read-groups
            :type hash-table)
   (%key :initarg :key
         :type hash-table
         :reader read-key)))


(defmethod apply-aggregation-function ((range group-by-states-collector)
                                       (function aggregation-function)
                                       &rest all &key key
                                       &allow-other-keys)
  (declare (ignore all))
  (let ((groups (copy-hash-table (read-groups range)))
        (old-groups (read-accumulated-args range))
        (label (access-label range))
        (extract-key (read-key range)))
    (cl-ds:traverse (lambda (x)
                      (let ((k (funcall extract-key x)))
                        (aggregate function
                                   (ensure (gethash k groups)
                                     (apply #'make-state function (gethash k old-groups)))
                                   (if (null key) x (funcall key x)))))
                    range)
    (maphash (lambda (key state)
               (push (state-result function state)
                     (gethash key old-groups))
               (push label
                     (gethash key old-groups)))
             groups)
    nil))


(defmethod apply-aggregation-function ((range group-by-proxy)
                                       (function aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (let ((groups (copy-hash-table (read-groups range)))
        (extract-key (read-key range)))
    ;; first, create groups…
    (cl-ds:traverse (lambda (x)
                      (let ((k (funcall extract-key x)))
                        (setf (gethash k groups) all)))
                    (read-original-range range))
    ;; next, initialize prior states…
    (let ((prior-stages (apply #'multi-aggregation-stages function all)))
      (unless (null prior-stages)
        (let ((collector (make-instance 'group-by-states-collector
                                        :groups (read-groups range)
                                        :accumulated-args groups
                                        :original-range (read-original-range range)
                                        :key (read-key range))))
          (iterate
            (for (label . stage) in prior-stages)
            (setf (access-label collector) label)
            (funcall stage collector)))))
    (let ((final-states (copy-hash-table (read-groups range) :size (hash-table-size groups))))
      (cl-ds:traverse (lambda (x)
                        (let ((k (funcall extract-key x)))
                          (aggregate function
                                     (ensure (gethash k final-states)
                                       (apply #'make-state function (gethash k groups)))
                                     (if (null key) x (funcall key x)))))
                      (read-original-range range))
      (maphash (lambda (key state)
                 (setf (gethash key final-states) (state-result function state)))
               final-states)
      (make-hash-table-range final-states))))


(defmethod make-proxy ((range group-by-proxy)
                       class &rest all &key &allow-other-keys)
  (let ((original-range (read-original-range range))
        (original-groups (read-groups range)))
    (apply #'make-instance
           (type-of range)
           :original-range (apply #'make-proxy original-range class all)
           :groups (copy-hash-table original-groups)
           :key (read-key range))))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn group-by-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'forward-group-by-proxy
              :test test
              :key key))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn group-by-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'bidirectional-group-by-proxy
              :test test
              :key key))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn group-by-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'random-access-group-by-proxy
              :test test
              :key key))
