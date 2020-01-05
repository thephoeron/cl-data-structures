(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defun insert-into-tree (tree root element)
  (declare (optimize (debug 3)))
  (let ((stack (list (cons root nil))))
    (iterate
      (with node = root)
      (for next = (cf-insert tree node element))
      (while (consp next))
      (push next stack)
      (setf node (car next)))
    (iterate
      (for cell on stack)
      (for p-cell previous cell)
      (for p-node = (caar p-cell))
      (for ((node . position) . rest) = cell)
      (unless (or (null p-node)
                  (typep p-node 'fundamental-cf-node))
        (assert (vectorp p-node))
        (assert position)
        (absorb-nodes tree node p-node position))
      (for at-root = (endp rest))
      (if (needs-split-p tree node)
          (let ((splitted (split tree node)))
            (if at-root
                (let ((new-root (make-subtree tree)))
                  (absorb-nodes tree new-root splitted)
                  (leave new-root))
                (setf (caar cell) splitted)))
          (leave root)))))


(defun single-thread-bubble-grouping (tree data)
  (iterate
    (with root = (make-leaf tree))
    (for d in-vector data)
    (setf root (insert-into-tree tree root d))
    (finally (return root))))


(defun gather-leafs (tree root &key (key #'identity))
  (lret ((result (vect)))
    (visit-leafs tree root
                 (rcurry #'vector-push-extend result)
                 :key key)))


(defun draw-sample (tree data)
  (let* ((length (length data))
         (sample-size (min length
                           (read-parallel-sample-size tree)))
         (distance-function (read-distance-function tree))
         (sample (make-array sample-size))
         (distances (make-array sample-size)))
    (declare (type fixnum sample-size length))
    (map-into sample (cl-ds.utils:lazy-shuffle 0 length))
    (cl-ds.utils:transform (lambda (i) (aref data i)) sample)
    (iterate outer
      (declare (type fixnum i))
      (for i from 0 below length)
      (for elt = (aref data i))
      (map-into distances
                (lambda (sample)
                  (funcall distance-function
                           sample
                           elt))
                sample)
      (iterate
        (for i from 0 below sample-size)
        (iterate
          (for j from 0 below i)
          (in outer (sum (abs (- (aref distances i)
                                 (aref distances j)))
                         into total))))
      (finally (return-from outer (cons total sample))))))


(defun select-parallel-samples (tree data)
  (~> tree
      read-parallel-samples-count
      make-array
      (lparallel:pmap-into (lambda () (draw-sample tree data)))
      (extremum #'> :key #'car)
      cdr))


(defun select-parallel-global-partitions (tree data samples)
  (let* ((distance-function (read-distance-function tree))
         (samples-count (length samples))
         (result (map-into (make-array samples-count) #'vect))
         (locks (map-into (make-array samples-count) #'bt:make-lock)))
    (lparallel:pmap nil
                    (lambda (x)
                      (iterate
                        (for i from 0 below samples-count)
                        (for sample = (aref sample i))
                        (for distance = (funcall distance-function sample x))
                        (finding i minimizing distance into destination)
                        (finally (bt:with-lock-held ((aref locks destination))
                                   (vector-push-extend x (aref result destination))))))
                    data)
    result))


(defun parallel-bubble-grouping (tree data)
  (lret ((result (make-subtree tree)))
    (~>> (select-parallel-samples tree data)
         (select-parallel-global-partitions tree data)
         (lparallel:pmap 'vector
                         (curry #'single-thread-bubble-grouping tree))
         (absorb-nodes tree result))))
