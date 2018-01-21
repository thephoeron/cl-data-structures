(in-package :cl-user)
(defpackage rrb-test-suite
  (:use :prove :cl))
(in-package :rrb-test-suite)
(cl-ds.utils:import-all-package-symbols :cl-data-structures.common.rrb :rrb-test-suite)

(progn
  (prove:plan 2)
  (is (tail-offset +maximum-children-count+) 0)
  (is (tail-offset 500) 480)
  (prove:finalize))

(let* ((tail (make-array +maximum-children-count+
                         :initial-element nil))
       (tag (make-ownership-tag))
       (container (make-instance 'rrb-container
                                 :tail tail
                                 :size 0)))
  (map-into tail #'identity
            (iota +maximum-children-count+))
  (let ((new-root (insert-tail container
                               tag
                               #'copy-on-write
                               tail)))
    (is (access-shift container) 0)
    (is (rrb-node-content new-root) tail :test #'eq)
    (is (rrb-node-ownership-tag new-root) tag)
    (let ((another-tail (make-array +maximum-children-count+
                                    :initial-element nil)))
      (map-into another-tail #'identity
                (iota +maximum-children-count+
                      :start +maximum-children-count+))
      (setf (access-root container) new-root)
      (setf (access-size container) +maximum-children-count+)
      (setf (access-tail-size container) 0)
      (let ((another-root (insert-tail container
                                       tag
                                       #'copy-on-write
                                       another-tail)))
        (print another-root)
        (is (rrb-node-ownership-tag another-root) tag)
        (is-type (~> another-root rrb-node-content (aref 1))
                 'rrb-node)
        (is-type (~> another-root rrb-node-content (aref 0))
                 'rrb-node)
        (is (~> another-root rrb-node-content (aref 1) rrb-node-content)
            another-tail
            :test #'eq)
        (ok (not (eq another-root new-root)))
        (setf (access-root container) another-root))))
  (setf (access-shift container) 1)
  (setf (access-size container) (* +maximum-children-count+ +maximum-children-count+))
  (iterate
    (for i from 2 below +maximum-children-count+)
    (iterate
      (for c from 0 below +maximum-children-count+)
      (setf (~> container access-root rrb-node-content (aref i))
            (make-rrb-node :content
                           (map 'vector #'identity
                                (iota +maximum-children-count+
                                      :start (* i +maximum-children-count+)))
                           :ownership-tag tag))))
  (let* ((another-tail (map 'vector #'identity
                            (iota +maximum-children-count+
                                  :start (expt +maximum-children-count+ 2))))
         (another-root (insert-tail container
                                    tag
                                    #'copy-on-write
                                    another-tail))
         (result (with-collector (result)
                   (labels ((impl (node)
                              (if (rrb-node-p node)
                                  (iterate
                                    (for elt in-vector (rrb-node-content node))
                                    (until (null elt))
                                    (impl elt))
                                  (result node))))
                     (impl another-root)))))
    (setf (access-size container) (+ (expt +maximum-children-count+ 2) +maximum-children-count+)
          (access-shift container) 2
          (access-root container) another-root)
    (is result (iota (+ (expt +maximum-children-count+ 2) +maximum-children-count+))
        :test #'equal)
    (is (iterate
          (for elt in-vector (rrb-node-content another-root))
          (counting elt))
        2)
    (iterate
      (for i from 0 below (+ (expt +maximum-children-count+ 2) +maximum-children-count+))
      (is (rrb-at container i) i)))
  (let ((old-root (access-root container))
        (another-root (remove-tail container
                                   tag
                                   #'copy-on-write-without-tail)))
    (setf (access-shift container) 1
          (access-root container) another-root)
    (decf (access-size container) +maximum-children-count+)
    (is another-root
        (~> old-root rrb-node-content (aref 0)))
    (setf another-root (remove-tail container
                                    tag
                                    #'copy-on-write-without-tail))
    (iterate
      (for i from 0 below (1- +maximum-children-count+))
      (is-type (~> another-root rrb-node-content (aref i))
               'rrb-node))
    (is (~> another-root rrb-node-content (aref (1- +maximum-children-count+)))
        nil)))
