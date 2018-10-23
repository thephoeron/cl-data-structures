(in-package :cl-user)
(defpackage sparse-rrb-vector-tests
  (:use :cl :prove :cl-data-structures.aux-package)
  (:shadowing-import-from :iterate :collecting :summing :in))
(in-package :sparse-rrb-vector-tests)

(plan 130295)

(defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:grow-function)
                                    (container (eql :mock))
                                    bucket
                                    location
                                    &rest all)
  (declare (ignore all))
  (values location :ok t))

(defmethod cl-ds.meta:shrink-bucket! ((opreation cl-ds.meta:shrink-function)
                                      (container (eql :mock))
                                      bucket
                                      location
                                      &rest all)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket :ok t))

(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:grow-function)
                                   (container (eql :mock))
                                   bucket
                                   location
                                   &rest all)
  (declare (ignore all))
  (values location :ok t))

(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:grow-function)
                                   (container (eql :mock))
                                   location
                                   &rest all)
  (declare (ignore all))
  (values location :ok t))

(bind ((vector (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector
                              :tail nil))
       ((:values structure status)
        (cl-ds.dicts.srrb::set-in-tail! vector #'cl-ds:add! :mock 5 5 nil))
       (tail (cl-ds.dicts.srrb::access-tail vector))
       (tail-mask (cl-ds.dicts.srrb::access-tail-mask vector)))
  (is structure vector)
  (is status :ok)
  (is (aref tail 5) 5)
  (is tail-mask (ash 1 5)))

(let* ((tail (make-array cl-ds.common.rrb:+maximum-children-count+))
       (vector (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector
                              :tail tail
                              :tail-mask #b111
                              :index-bound 32)))
  (setf (aref tail 0) 1
        (aref tail 1) 2
        (aref tail 2) 3
        (aref tail 3) 4)
  (is (cl-ds:size vector) 3)
  (is (cl-ds.dicts.srrb::access-tree vector) nil)
  (cl-ds.dicts.srrb::insert-tail! vector)
  (is (cl-ds.dicts.srrb::access-tail-mask vector) 0)
  (is (cl-ds.dicts.srrb::access-shift vector) 0)
  (ok (cl-ds.dicts.srrb::access-tree vector))
  (is (cl-ds:size vector) 3)
  (is (cl-ds.dicts.srrb::access-tree-index-bound vector) cl-ds.common.rrb:+maximum-children-count+)
  (is (cl-ds.dicts.srrb::access-tree-size vector) 3)
  (let* ((tree (cl-ds.dicts.srrb::access-tree vector))
         (content (cl-ds.common.rrb:sparse-rrb-node-content tree)))
    (is (cl-ds.common.rrb:sparse-rrb-node-bitmask tree)
        #b111)
    (is (length content)
        3)
    (is (aref content 0) 1)
    (is (aref content 1) 2)
    (is (aref content 2) 3))
  (setf (cl-ds.dicts.srrb::access-tail vector) (make-array cl-ds.common.rrb:+maximum-children-count+
                                                           :initial-element 10)
        (cl-ds.dicts.srrb::access-tail-mask vector) #b1)
  (cl-ds.dicts.srrb::insert-tail! vector)
  (is (cl-ds.dicts.srrb::access-shift vector) 1)
  (is (cl-ds.dicts.srrb::access-tree-index-bound vector) (* 2 cl-ds.common.rrb:+maximum-children-count+))
  (let* ((tree (cl-ds.dicts.srrb::access-tree vector))
         (content (cl-ds.common.rrb:sparse-rrb-node-content tree)))
    (is (cl-ds.common.rrb:sparse-rrb-node-bitmask tree) #b11)
    (is (length content) 2))
  (setf (cl-ds.dicts.srrb::access-tail vector) (make-array cl-ds.common.rrb:+maximum-children-count+)
        (cl-ds.dicts.srrb::access-tail-mask vector) #b0)
  (cl-ds.dicts.srrb::insert-tail! vector)
  (setf (cl-ds.dicts.srrb::access-tail vector) (make-array cl-ds.common.rrb:+maximum-children-count+
                                                           :initial-element 15)
        (cl-ds.dicts.srrb::access-tail-mask vector) #b1)
  (cl-ds.dicts.srrb::insert-tail! vector)
  (let* ((tree (cl-ds.dicts.srrb::access-tree vector))
         (content (cl-ds.common.rrb:sparse-rrb-node-content tree))
         (bitmask (cl-ds.common.rrb:sparse-rrb-node-bitmask tree)))
    (is (length content) 3)
    (is bitmask #b1011)
    (let ((c (map 'vector #'cl-ds.common.rrb:sparse-rrb-node-content content)))
      (is (~> c (aref 0) (aref 0)) 1)
      (is (~> c (aref 0) (aref 1)) 2)
      (is (~> c (aref 0) (aref 2)) 3)
      (is (~> c (aref 1) (aref 0)) 10)
      (is (~> c (aref 2) (aref 0)) 15)))
  (is (cl-ds.dicts.srrb::access-shift vector) 1)
  (is (cl-ds:at vector 0) 1)
  (is (cl-ds:at vector 1) 2)
  (is (cl-ds:at vector 2) 3)
  (is (cl-ds:at vector 32) 10)
  (is (cl-ds:at vector (* 32 3)) 15)
  (cl-ds.dicts.srrb::adjust-tree-to-new-size! vector 9999 nil)
  (is (cl-ds:at vector 0) 1)
  (is (cl-ds:at vector 1) 2)
  (is (cl-ds:at vector 2) 3)
  (is (cl-ds:at vector 32) 10)
  (is (cl-ds:at vector (* 32 3)) 15)
  (cl-ds.dicts.srrb::adjust-tree-to-new-size! vector 32 nil)
  (is (cl-ds.dicts.srrb::access-shift vector) 0)
  (let* ((tree (cl-ds.dicts.srrb::access-tree vector))
         (content (cl-ds.common.rrb:sparse-rrb-node-content tree)))
    (is (cl-ds.common.rrb:sparse-rrb-node-bitmask tree)
        #b111)
    (is (length content)
        3)
    (is (aref content 0) 1)
    (is (aref content 1) 2)
    (is (aref content 2) 3)))

(let* ((count 500)
       (container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector))
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list* (cl-ds:iota-range :to count))
                        cl-ds.alg:to-vector)))
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification #'(setf cl-ds:at) container :mock
                                      position :value point))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (setf input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list* (cl-ds.alg:shuffled-range 0 count))
                        cl-ds.alg:to-vector))
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification #'(setf cl-ds:at) container :mock
                                      position :value point))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (is (cl-ds.dicts.srrb::access-tree-index-bound container)
      (cl-ds.dicts.srrb::scan-index-bound container)))


(let ((shift (cl-ds.dicts.srrb::shift-for-position 47)))
  (is shift 1))


(let ((shift (cl-ds.dicts.srrb::shift-for-position 308)))
  (is shift 1))


(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
       (container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification #'(setf cl-ds:at) container :mock
                                      position :value point))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (iterate
    (repeat (1- (length input-data)))
    (for position = (car (aref input-data 0)))
    (for (values structure status) = (cl-ds.meta:position-modification
                                      #'cl-ds:erase! container :mock position))
    (is structure container)
    (is status :ok)
    (is (nth-value 1 (cl-ds:at container position)) nil)
    (cl-ds.utils:swapop input-data 0)
    (iterate
      (for (position . point) in-vector input-data)
      (is (cl-ds:at container position) point))))

(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
       (container (make-instance 'cl-ds.dicts.srrb::transactional-sparse-rrb-vector
                                 :ownership-tag (cl-ds.common.abstract:make-ownership-tag))))
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification #'(setf cl-ds:at) container :mock
                                      position :value point))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point)))

(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list* (cl-ds:iota-range))
                        cl-ds.alg:to-vector))
       (container (make-instance 'cl-ds.dicts.srrb::functional-sparse-rrb-vector)))
  (iterate
    (for (position . point) in-vector input-data)
    (setf container (cl-ds.meta:position-modification #'cl-ds:insert
                                                      container :mock
                                                      position :value point)))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (setf input-data (~>> (cl-ds.alg:shuffled-range 0 count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
  (iterate
    (for (position . point) in-vector input-data)
    (setf container (cl-ds.meta:position-modification #'cl-ds:insert
                                                      container :mock
                                                      position :value point))
    (is (cl-ds.dicts.srrb::access-tree-index-bound container)
        (cl-ds.dicts.srrb::scan-index-bound container)))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point)))

(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
       (container (make-instance 'cl-ds.dicts.srrb::functional-sparse-rrb-vector)))
  (iterate
    (for (position . point) in-vector input-data)
    (setf container (cl-ds.meta:position-modification #'cl-ds:insert
                                                      container :mock
                                                      position :value point)))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point)))

(finalize)
