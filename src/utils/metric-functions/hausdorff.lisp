(in-package #:cl-data-structures.utils.metric)


(defun hausdorff-metric (a b fn
                         &key
                           (element-type t)
                           (distance-matrix (make-array (list (length a)
                                                              (length b))
                                                        :element-type element-type)))
  (declare (type vector a) (type vector b)
           (optimize (speed 3)))
  (ensure-functionf fn)
  (iterate
    (for ea in-vector a)
    (for ia from 0)
    (iterate
      (for eb in-vector b)
      (for ib from 0)
      (setf (aref distance-matrix ia ib) (funcall fn ea eb))))
  (max (iterate
         (for i from 0 below (length a))
         (maximize (iterate
                     (for j from 0 below (length b))
                     (minimize (aref distance-matrix i j)))))
       (iterate
         (for i from 0 below (length b))
         (maximize (iterate
                     (for j from 0 below (length a))
                     (minimize (aref distance-matrix j i)))))))
