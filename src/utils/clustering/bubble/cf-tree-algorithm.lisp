(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defun insert-into-tree (tree root element)
  (let ((stack (vect (cons root nil))))
    (iterate
      (for node = (car (aref stack (1- (length stack)))))
      (for next = (cf-insert tree node element))
      (while (consp next))
      (vector-push-extend next stack))
    (iterate
      (for i from (1- (length stack)) downto 0)
      (for p-i previous i)
      (for cell = (aref stack i))
      (for (node . s-position) = cell)
      (for position = (if (null p-i)
                          nil
                          (~> stack (aref p-i) cdr)))
      (unless (or (null p-i)
                  (~> stack (aref p-i) car
                      (typep 'fundamental-cf-node)))
        (assert (~> stack (aref p-i) car vectorp))
        (assert position)
        (~> stack (aref p-i) car
            (absorb-nodes tree node _ position)))
      (for at-root = (zerop i))
      (if (needs-split-p tree node)
          (let ((splitted (split tree node)))
            (assert (vectorp splitted))
            (if at-root
                (let ((new-root (make-subtree tree)))
                  (absorb-nodes tree new-root splitted)
                  (leave new-root))
                (setf (car (aref stack i)) splitted)))
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
