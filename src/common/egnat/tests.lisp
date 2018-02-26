(in-package :cl-user)
(defpackage egnat-tests (:use :prove :cl :iterate))
(in-package :egnat-tests)


(defmethod cl-ds:make-bucket (operation container content &rest all)
  (declare (ignore all))
  content)


(plan 1)
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
    (is (length (cl-ds.common.egnat::read-content root)) 5)))
(finalize)
