(in-package :cl-user)
(defpackage egnat-tests (:use :prove :cl :iterate))
(in-package :egnat-tests)


(defmethod cl-ds:make-bucket (operation container content &rest all)
  (declare (ignore all))
  (let ((result (make-array (cl-ds:size content))))
    (iterate
      (for i from 0 below (cl-ds:size content))
      (setf (aref result i) (cl-ds:at content i)))
    result))


(plan 3)

(let ((container (make 'cl-ds.common.egnat:fundamental-egnat-container
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
      (is (sort data #'<) (sort content #'<) :test #'serapeum:vector=))))

(finalize)
