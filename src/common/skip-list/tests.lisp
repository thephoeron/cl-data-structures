(cl:in-package #:cl-user)
(defpackage skip-list-tests (:use :cl :cl-data-structures.aux-package))
(in-package #:skip-list-tests)

(prove:plan 6)

(let* ((node1 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector nil)
               :content 3))
       (node2 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node1)
               :content 2))
       (node3 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node2)
               :content 1))
       (found (cl-ds.common.skip-list:locate-node
               (vector node3)
               2
               #'<)))
  (prove:is (aref found 0) node2))

(bind ((node1 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector nil)
               :content 3))
       (node2 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node1)
               :content 2))
       (node3 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node2)
               :content 1))
       ((:values found prev) (cl-ds.common.skip-list:locate-node
                              (vector node3)
                              5
                              #'<)))
  (prove:is (aref found 0) nil)
  (prove:is (aref prev 0) node1))

(let* ((node1 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector nil)
               :content 3))
       (node2 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node1)
               :content 2))
       (node3 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node2)
               :content 1))
       (found (cl-ds.common.skip-list:locate-node
               (vector node3)
               0
               #'<)))
  (prove:is (aref found 0) node3))


(bind ((node1 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector nil)
               :content 5))
       (node2 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node1)
               :content 2))
       (node3 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node2)
               :content 1))
       ((:values found prev)
        (cl-ds.common.skip-list:locate-node
         (vector node3)
         3
         #'<)))
  (prove:is (aref found 0) node1)
  (prove:is (aref prev 0) node2))


(prove:finalize)
