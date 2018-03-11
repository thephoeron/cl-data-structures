(in-package :cl-user)
(defpackage egnat-tests (:use :prove :cl :iterate))
(in-package :egnat-tests)


(defmethod cl-ds:make-bucket (operation container content
                              &rest all)
  (declare (ignore all))
  content)


(plan 12)

(let ((container (make-instance
                  'cl-ds.common.egnat:fundamental-egnat-container
                  :branching-factor 5
                  :metric-fn #'logxor
                  :same-fn #'=
                  :metric-type 'fixnum
                  :content-count-in-node 5))
      (data (coerce (iterate
                      (with generator = (cl-ds.utils:lazy-shuffle 0 5000))
                      (repeat 50)
                      (collect (funcall generator)))
                    'vector)))
  (let ((root (cl-ds.common.egnat::make-egnat-tree container nil nil data))
        (content (serapeum:vect)))
    (is (length (cl-ds.common.egnat::read-content root)) 5)
    (labels ((impl (root)
               (unless (null root)
                 (iterate
                   (for c in-vector (cl-ds.common.egnat::read-content root))
                   (vector-push-extend c content)))
               (if (null root)
                   0
                   (+ (length (cl-ds.common.egnat::read-content root))
                      (reduce #'+
                              (cl-ds.common.egnat::read-children root)
                              :key #'impl)))))
      (is (impl root) 50)
      (is (sort data #'<) (sort content #'<) :test #'serapeum:vector=))
    (let* ((children (cl-ds.common.egnat::read-children root))
           (close-range (cl-ds.common.egnat::read-close-range root))
           (distant-range (cl-ds.common.egnat::read-distant-range root))
           (selection (aref data 10))
           (subtrees (cl-ds.common.egnat::prune-subtrees container
                                                         children
                                                         close-range
                                                         distant-range
                                                         selection
                                                         5)))
      (labels ((impl (root)
                 (when root
                   (or
                    (find selection
                          (cl-ds.common.egnat::read-content root))
                    (some #'impl
                          (cl-ds.common.egnat::read-children root))))))
        (ok (iterate
              (for i in-vector subtrees)
              (for tree in-vector children)
              (for result = (impl tree))
              (count result)))))
    (let ((range (make-instance 'cl-ds.common.egnat::egnat-range
                                :container container
                                :stack (list (cons root 0)))))
      (iterate
        (for (values value more) = (cl-ds:consume-front range))
        (while more)
        (collect value into result)
        (finally (is (serapeum:~> result (sort #'<) (coerce 'vector))
                     data
                     :test #'serapeum:vector=))))
    (let ((range (make-instance 'cl-ds.common.egnat::egnat-range-around
                                :container container
                                :margin 5
                                :near (aref data 10)
                                :stack (list (cons root 0)))))
      (iterate
        (for (values value more) = (cl-ds:consume-front range))
        (while more)
        (collect value into result)
        (finally (ok (every (lambda (x) (<= (logxor x (aref data 10)) 5))
                            result)))))
    (setf (cl-ds.common.egnat::access-root container) root)
    (multiple-value-bind (possible-paths found last-node)
        (cl-ds.common.egnat::find-destination-node container
                                                   (aref data 10))
      (isnt (hash-table-count possible-paths) 0)
      (ok found)
      (ok (find (aref data 10)
                (cl-ds.common.egnat::read-content last-node)))
      (let ((node nil))
        (cl-ds.common.egnat::walk-path (lambda (x) (setf node x))
                                       last-node
                                       possible-paths)
        (is (car node) root :test #'eq)))))


(is-error (make-instance 'cl-ds.common.egnat:fundamental-egnat-container
                         :branching-factor 0
                         :content-count-in-node 1)
          'cl-ds:initialization-out-of-bounds)
(is-error (make-instance 'cl-ds.common.egnat:fundamental-egnat-container
                         :branching-factor 5
                         :content-count-in-node 0)
          'cl-ds:initialization-out-of-bounds)

(finalize)
