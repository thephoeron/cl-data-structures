(in-package #:cl-data-structures.dicts.srrb)


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure transactional-sparse-rrb-vector)
                                             container
                                             position &rest all &key value)
  (let ((tree-bound (access-tree-index-bound structure)))
    (cond ((negative-fixnum-p position)
           (error 'cl-ds:argument-out-of-bounds
                  :argument 'position
                  :value position
                  :bounds "Must be non-negative"
                  :text "Sparse vector index can not be negative."))
          ((< position tree-bound)
           (transactional-grow-tree! operation structure
                                     container position
                                     all value))
          ((< position (access-index-bound structure))
           (set-in-tail! structure operation container
                         (logandc2 position cl-ds.common.rrb:+tail-mask+)
                         value all))
          (t (bind (((:values bucket status changed)
                     (apply #'cl-ds.meta:make-bucket
                            operation container value all)))
               (when changed
                 (let* ((new-tree-bound (logand position
                                                cl-ds.common.rrb:+tail-mask+))
                        (ownership-tag (cl-ds.common.abstract:read-ownership-tag
                                        structure)))
                   (transactional-insert-tail! structure ownership-tag)
                   (adjust-tree-to-new-size! structure new-tree-bound
                                             ownership-tag)
                   (setf (access-tree-index-bound structure) new-tree-bound
                         (access-index-bound structure)
                         (+ new-tree-bound
                            cl-ds.common.rrb:+maximum-children-count+)))
                 (assert (> (access-index-bound structure)
                            (access-tree-index-bound structure)))
                 (let* ((offset (logandc2 position
                                          cl-ds.common.rrb:+tail-mask+))
                        (tail-mask (ash 1 offset))
                        (tail (cl-ds.common.rrb:make-node-content
                               (read-element-type structure))))
                   (setf (aref tail offset) bucket
                         (access-tail structure) tail
                         (access-tail-mask structure) tail-mask)))
               (values structure status))))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure mutable-sparse-rrb-vector)
                                             container
                                             position &rest all &key value)
  (let ((tree-bound (access-tree-index-bound structure)))
    (cond ((negative-fixnum-p position)
           (error 'cl-ds:argument-out-of-bounds
                  :argument 'position
                  :value position
                  :bounds "Must be non-negative"
                  :text "Sparse vector index can not be negative."))
          ((< position tree-bound)
           (destructive-grow-tree! operation structure
                                   container position
                                   all value))
          ((< position (access-index-bound structure))
           (set-in-tail! structure operation container
                         (logandc2 position cl-ds.common.rrb:+tail-mask+)
                         value all))
          (t (bind (((:values bucket status changed)
                     (apply #'cl-ds.meta:make-bucket
                            operation container value all)))
               (when changed
                 (let ((new-tree-bound (logand position
                                               cl-ds.common.rrb:+tail-mask+)))
                   (insert-tail! structure)
                   (adjust-tree-to-new-size! structure new-tree-bound nil)
                   (setf (access-tree-index-bound structure) new-tree-bound
                         (access-index-bound structure)
                         (+ new-tree-bound
                            cl-ds.common.rrb:+maximum-children-count+)))
                 (assert (> (access-index-bound structure)
                            (access-tree-index-bound structure)))
                 (let* ((offset (logandc2 position
                                          cl-ds.common.rrb:+tail-mask+))
                        (tail-mask (ash 1 offset))
                        (tail (cl-ds.common.rrb:make-node-content
                               (read-element-type structure))))
                   (setf (aref tail offset) bucket
                         (access-tail structure) tail
                         (access-tail-mask structure) tail-mask)))
               (values structure status))))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure functional-sparse-rrb-vector)
                                             container
                                             position &rest all &key value)
  (let ((tree-bound (access-tree-index-bound structure)))
    (cond ((negative-fixnum-p position)
           (error 'cl-ds:argument-out-of-bounds
                  :argument 'position
                  :value position
                  :bounds "Must be non-negative"
                  :text "Sparse vector index can not be negative."))
          ((< position tree-bound)
           (grow-tree operation structure
                      container position
                      all value))
          ((< position (access-index-bound structure))
           (set-in-tail structure operation container
                        (logandc2 position cl-ds.common.rrb:+tail-mask+)
                        value all))
          (t (bind (((:values bucket status changed)
                     (apply #'cl-ds.meta:make-bucket
                            operation container value all)))
               (if changed
                 (let ((new-tree-bound (logand position
                                                cl-ds.common.rrb:+tail-mask+))
                       (new-structure (insert-tail structure)))
                   (adjust-tree-to-new-size! new-structure new-tree-bound nil)
                   (setf (access-tree-index-bound new-structure) new-tree-bound
                         (access-index-bound new-structure)
                         (+ new-tree-bound
                            cl-ds.common.rrb:+maximum-children-count+))
                   (assert (> (access-index-bound new-structure)
                              (access-tree-index-bound new-structure)))
                   (let* ((offset (logandc2 position
                                            cl-ds.common.rrb:+tail-mask+))
                          (tail-mask (ash 1 offset))
                          (tail (cl-ds.common.rrb:make-node-content
                                 (read-element-type structure))))
                     (setf (aref tail offset) bucket
                           (access-tail new-structure) tail
                           (access-tail-mask new-structure) tail-mask)
                     (values new-structure status)))
                 (values structure status)))))))


(defmethod cl-ds:size ((vect fundamental-sparse-rrb-vector))
  (+ (access-tree-size vect) (logcount (access-tail-mask vect))))


(defmethod cl-ds:at ((vect fundamental-sparse-rrb-vector)
                     position
                     &rest more-positions)
  (declare (optimize (debug 3)))
  (cl-ds:assert-one-dimension more-positions)
  (check-type position fixnum)
  (let ((bound (access-index-bound vect))
        (tree-bound (access-tree-index-bound vect)))
    (cond ((not (< -1 position bound))
           (values nil nil))
          ((< position tree-bound)
           (iterate
             (with tree = (access-tree vect))
             (with node = tree)
             (with shift = (access-shift vect))
             (for byte-position
                  from (* cl-ds.common.rrb:+bit-count+
                          shift)
                  downto 0
                  by cl-ds.common.rrb:+bit-count+)
             (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                           position))
             (for present =
                  (cl-ds.common.rrb:sparse-rrb-node-contains node
                                                             i))
             (unless present
               (leave (values nil nil)))
             (setf node (cl-ds.common.rrb:sparse-nref node i))
             (finally (return (values node t)))))
          (t (let* ((offset (- position tree-bound))
                    (present (ldb-test (byte 1 offset)
                                       (access-tail-mask vect))))
               (if present
                   (values (aref (access-tail vect) offset) t)
                   (values nil nil)))))))


(defmethod cl-ds:become-mutable ((container fundamental-sparse-rrb-vector))
  (make 'mutable-sparse-rrb-vector
        :tree (access-tree container)
        :tail (access-tail container)
        :tail-mask (access-tail-mask container)
        :shift (access-shift container)
        :tree-size (access-tree-size container)
        :tree-index-bound (access-tree-index-bound container)
        :index-bound (access-index-bound container)
        :element-type (read-element-type container)))


(defmethod cl-ds:become-transactional ((container fundamental-sparse-rrb-vector))
  (make 'transactional-sparse-rrb-vector
        :tree (access-tree container)
        :tail (copy-array (access-tail container))
        :tail-mask (access-tail-mask container)
        :shift (access-shift container)
        :tree-size (access-tree-size container)
        :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
        :tree-index-bound (access-tree-index-bound container)
        :index-bound (access-index-bound container)
        :element-type (read-element-type container)))


(defun make-transactional-sparse-rrb-vector (&key (element-type t))
  (make-instance 'transactional-sparse-rrb-vector
                 :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
                 :element-type element-type))


(defun make-mutable-sparse-rrb-vector (&key (element-type t))
  (make-instance 'mutable-sparse-rrb-vector
                 :element-type element-type))


(defun make-functional-sparse-rrb-vector (&key (element-type t))
  (make-instance 'functional-sparse-rrb-vector
                 :element-type element-type))
