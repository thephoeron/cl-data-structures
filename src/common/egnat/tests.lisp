(in-package :cl-user)
(defpackage egnat-tests (:use :prove :cl :iterate))
(in-package :egnat-tests)


(defmethod cl-ds:make-bucket (operation container content
                              &rest all &key head)
  (declare (ignore all))
  (let ((result (make-array (1+ (cl-ds:size content)))))
    (setf (aref result 0) head)
    (iterate
      (for i from 0 below (cl-ds:size content))
      (setf (aref result (1+ i)) (cl-ds:at content i)))
    result))


(plan 6)

(let ((container (make-instance
                  'cl-ds.common.egnat:fundamental-egnat-container
                  :branching-factor 5
                  :metric-fn #'logxor
                  :metric-type 'fixnum
                  :content-count-in-node 5))
      (data (coerce (iterate
                      (with generator = (cl-ds.utils:lazy-shuffle 0 5000))
                      (repeat 50)
                      (collect (funcall generator)))
                    'vector)))
  (let ((root (cl-ds.common.egnat::make-egnat-tree nil container nil data))
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
           (subtrees (cl-ds.common.egnat::prune-subtrees children
                                                         close-range
                                                         distant-range
                                                         selection
                                                         5
                                                         #'logxor)))
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
              (count result)))))))


(is-error (make-instance 'cl-ds.common.egnat:fundamental-egnat-container
                         :branching-factor 0
                         :content-count-in-node 1)
          'cl-ds:initialization-out-of-bounds)
(is-error (make-instance 'cl-ds.common.egnat:fundamental-egnat-container
                         :branching-factor 1
                         :content-count-in-node 0)
          'cl-ds:initialization-out-of-bounds)

(finalize)
