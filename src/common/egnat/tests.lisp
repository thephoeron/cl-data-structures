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


(plan 2)

(let ((container (make-instance 'cl-ds.common.egnat::fundamental-egnat
                                :branching-factor 5
                                :metric-fn #'logxor
                                :metric-type 'fixnum
                                :content-count-in-node 5))
      (data (coerce (iterate
                      (with generator = (cl-ds.utils:lazy-shuffle 0 5000))
                      (repeat 50)
                      (collect (funcall generator)))
                    'vector)))
  (let ((root (cl-ds.common.egnat::make-egnat-tree nil container nil data)))
    (is (length (cl-ds.common.egnat::read-content root)) 5)
    (labels ((impl (root)
               (if (null root)
                   0
                   (+ (length (cl-ds.common.egnat::read-content root))
                      (reduce #'+
                              (cl-ds.common.egnat::read-children root)
                              :key #'impl)))))
      (is (impl root) 50))))

(finalize)
