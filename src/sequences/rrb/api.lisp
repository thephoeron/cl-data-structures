(in-package #:cl-data-structures.sequences.rrb-vector)


(defclass rrb-vector (cl-ds.common.rrb::rrb-container)
  ())


(defclass functional-rrb-vector (rrb-vector
                                 cl-ds.seqs:functional-sequence)
  ())


(defclass mutable-rrb-vector (rrb-vector
                              cl-ds.seqs:mutable-sequence)
  ())


(defclass transactional-rrb-vector (rrb-vector
                                    cl-ds.seqs:transactional-sequence)
  ())


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:take-out!-function)
                                             (container transactional-rrb-vector)
                                             location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (debug 0) (safety 1)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
         (result-status nil)
         (tail-change 0)
         ((:dflet shrink-bucket (bucket))
          (multiple-value-bind (bucket status changed)
              (apply #'cl-ds.meta:shrink-bucket
                     operation
                     container
                     bucket
                     location
                     rest)
            (setf result-status status)
            (unless changed
              (return-from cl-ds.meta:position-modification (values container status)))
            (when (cl-ds.meta:null-bucket-p bucket)
              (decf tail-change))
            bucket)))
    (if (zerop tail-size)
        (if (zerop (cl-ds.common.rrb:access-size container))
            (error 'cl-ds:empty-container :text "Can't take-out from empty container.")
            (bind (((:values new-root tail shift-decreased)
                    (cl-ds.common.rrb:remove-tail container))
                   (new-shift (if shift-decreased
                                  (1- (cl-ds.common.rrb:access-shift container))
                                  (cl-ds.common.rrb:access-shift container)))
                   (new-size (max 0 (- (cl-ds.common.rrb:access-size container)
                                       cl-ds.common.rrb:+maximum-children-count+)))
                   (new-tail (~> tail cl-ds.common.rrb:rrb-node-content))
                   (tail-size (if (null new-root)
                                  (cl-ds.common.rrb:access-size container)
                                  cl-ds.common.rrb:+maximum-children-count+))
                   (new-last-index (1- (+ tail-change tail-size)))
                   (new-bucket (~> new-tail
                                   (aref (1- cl-ds.common.rrb:+maximum-children-count+))
                                   shrink-bucket)))
              (cond ((and (cl-ds.meta:null-bucket-p new-bucket)
                          (zerop (+ tail-change tail-size)))
                     (setf new-tail nil))
                    ((~> new-bucket cl-ds.meta:null-bucket-p not)
                     (setf new-tail (copy-array new-tail)
                           (aref new-tail new-last-index) new-bucket))
                    (t (setf new-tail (copy-array new-tail)
                             (aref new-tail new-last-index) nil)))
              (assert (or new-root new-tail (zerop new-size)))
              (assert (<= 0 tail-size +maximum-children-count+))
              (setf (cl-ds.common.rrb:access-root container) new-root
                    (cl-ds.common.rrb:access-tail container) new-tail
                    (cl-ds.common.rrb:access-tail-size container) (+ tail-size tail-change)
                    (cl-ds.common.rrb:access-size container) new-size
                    (cl-ds.common.rrb:access-shift container) new-shift)))
        (let* ((tail (cl-ds.common.rrb:access-tail container))
               (new-bucket (shrink-bucket (aref tail (1- tail-size)))))
          (unless (cl-ds.meta:null-bucket-p new-bucket)
            (setf (aref tail (+ tail-change tail-size)) new-bucket))
          (setf (cl-ds.common.rrb:access-tail-size container)
                (+ (cl-ds.common.rrb:access-tail-size container)
                   tail-change))))
    (values container result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:take-out!-function)
                                             (container mutable-rrb-vector)
                                             location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (debug 0) (safety 1)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
         (result-status nil)
         (tail-change 0)
         ((:dflet shrink-bucket (bucket))
          (multiple-value-bind (bucket status changed)
              (apply #'cl-ds.meta:shrink-bucket!
                     operation
                     container
                     bucket
                     location
                     rest)
            (setf result-status status)
            (unless changed
              (return-from cl-ds.meta:position-modification (values container status)))
            (when (cl-ds.meta:null-bucket-p bucket)
              (decf tail-change))
            bucket)))
    (if (zerop tail-size)
        (if (zerop (cl-ds.common.rrb:access-size container))
            (error 'cl-ds:empty-container :text "Can't take-out from empty container.")
            (bind (((:values new-root tail shift-decreased)
                    (cl-ds.common.rrb:remove-tail container))
                   (new-shift (if shift-decreased
                                  (1- (cl-ds.common.rrb:access-shift container))
                                  (cl-ds.common.rrb:access-shift container)))
                   (new-size (max 0 (- (cl-ds.common.rrb:access-size container)
                                       cl-ds.common.rrb:+maximum-children-count+)))
                   (new-tail (~> tail cl-ds.common.rrb:rrb-node-content))
                   (tail-size (if (null new-root)
                                  (cl-ds.common.rrb:access-size container)
                                  cl-ds.common.rrb:+maximum-children-count+))
                   (last-index (1- (+ tail-change tail-size)))
                   (new-bucket (~> new-tail
                                   (aref (1- cl-ds.common.rrb:+maximum-children-count+))
                                   shrink-bucket)))
              (cond ((and (cl-ds.meta:null-bucket-p new-bucket) (zerop (+ tail-change tail-size)))
                     (setf new-tail nil))
                    ((~> new-bucket cl-ds.meta:null-bucket-p not)
                     (setf (aref new-tail last-index) new-bucket))
                    (t (setf (aref new-tail last-index) nil)))
              (assert (or new-root new-tail (zerop new-size)))
              (assert (<= 0 tail-size +maximum-children-count+))
              (setf (cl-ds.common.rrb:access-size container) new-size
                    (cl-ds.common.rrb:access-tail-size container) (+ tail-size
                                                                     tail-change)
                    (cl-ds.common.rrb:access-tail container) new-tail
                    (cl-ds.common.rrb:access-root container) new-root
                    (cl-ds.common.rrb:access-shift container) new-shift)))
        (bind ((tail (cl-ds.common.rrb:access-tail container))
               (last-index (1- tail-size))
               (new-bucket (shrink-bucket (aref tail last-index))))
          (if (cl-ds.meta:null-bucket-p new-bucket)
              (setf (aref tail last-index) nil
                    (cl-ds.common.rrb:access-tail-size container) last-index)
              (setf (aref tail last-index) new-bucket))))
    (values container result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (container mutable-rrb-vector)
                                             index &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (safety 1)
                     (space 0) (debug 0)))
  (bind ((size (cl-ds.common.rrb:access-size container))
         (result-status nil)
         (last-index (ldb (byte cl-ds.common.rrb:+bit-count+ 0) index))
         ((:dflet change-bucket (bucket))
          (multiple-value-bind (node status changed)
              (apply #'cl-ds.meta:grow-bucket!
                     operation
                     container
                     bucket
                     index
                     rest)
            (unless changed
              (return-from cl-ds.meta:position-modification
                (values container status)))
            (setf result-status status)
            node)))
    (unless (> (cl-ds:size container) index)
      (error 'cl-ds:argument-out-of-bounds
             :argument 'index
             :bounds (list 0 (cl-ds:size container))
             :value index
             :text "Index out of range."))
    (if (< index size)
        (let* ((node (iterate
                       (with shift = (cl-ds.common.rrb:access-shift container))
                       (repeat shift)
                       (for position
                            from (* cl-ds.common.rrb:+bit-count+ shift)
                            downto 0
                            by cl-ds.common.rrb:+bit-count+)
                       (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ position) index))
                       (for node
                            initially (cl-ds.common.rrb:access-root container)
                            then (~> node cl-ds.common.rrb:rrb-node-content (aref i)))
                       (finally (return node))))
               (last-array (cl-ds.common.rrb:rrb-node-content node))
               (bucket (change-bucket (aref last-array last-index))))
          (setf (aref last-array last-index) bucket))
        (let* ((offset (- index size))
               (tail (cl-ds.common.rrb:access-tail container))
               (bucket (change-bucket (aref tail offset))))
          (setf (aref tail offset) bucket)))
    (values container result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:put!-function)
                                             (container mutable-rrb-vector)
                                             location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (safety 1)
                     (debug 0) (space 0)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
         (tail (cl-ds.common.rrb:access-tail container))
         ((:values new-bucket status changed) (apply #'cl-ds.meta:make-bucket
                                                     operation
                                                     container
                                                     location
                                                     rest)))
    (unless changed
      (return-from cl-ds.meta:position-modification (values container status)))
    (if (eql tail-size +maximum-children-count+)
        (bind ((new-tail (~> container
                             cl-ds.common.rrb:read-element-type
                             cl-ds.common.rrb:make-node-content))
               ((:values new-root shift-increased)
                (cl-ds.common.rrb:insert-tail container
                                              (cl-ds.common.abstract:read-ownership-tag container)
                                              #'cl-ds.common.rrb:destructive-write
                                              tail)))
          (setf (aref new-tail 0) new-bucket
                (cl-ds.common.rrb:access-tail container) new-tail
                (cl-ds.common.rrb:access-root container) new-root
                (cl-ds.common.rrb:access-tail-size container) 1)
          (when shift-increased
            (incf (cl-ds.common.rrb:access-shift container)))
          (incf (cl-ds.common.rrb:access-size container) +maximum-children-count+)
          (values container status))
        (progn
          (setf tail (or tail (cl-ds.common.rrb:make-node-content))
                (aref tail tail-size) new-bucket
                (cl-ds.common.rrb:access-tail container) tail)
          (incf (cl-ds.common.rrb:access-tail-size container))
          (values container status)))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:functional-put-function)
                                             (container functional-rrb-vector)
                                             location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (debug 0) (safety 1)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
         (tag (cl-ds.common.abstract:make-ownership-tag))
         (tail-change 1)
         ((:values new-bucket status changed) (apply #'cl-ds.meta:make-bucket
                                                     operation
                                                     container
                                                     location
                                                     rest)))
    (unless changed
      (return-from cl-ds.meta:position-modification (values container status)))
    (if (eql tail-size +maximum-children-count+)
        (bind ((new-tail (~> container
                             cl-ds.common.rrb:read-element-type
                             cl-ds.common.rrb:make-node-content))
               ((:values new-root shift-increased)
                (cl-ds.common.rrb:insert-tail container
                                              tag
                                              #'cl-ds.common.rrb:copy-on-write
                                              (cl-ds.common.rrb:access-tail container))))
          (setf (aref new-tail 0) new-bucket)
          (make 'functional-rrb-vector
                :root new-root
                :tail new-tail
                :ownership-tag tag
                :tail-size tail-change
                :size (+ +maximum-children-count+
                         (cl-ds.common.rrb:access-size container))
                :tail new-tail
                :shift (if shift-increased
                           (1+ (cl-ds.common.rrb:access-shift container))
                           (cl-ds.common.rrb:access-shift container))))
        (values
         (make 'functional-rrb-vector
               :root (cl-ds.common.rrb:access-root container)
               :tail (let* ((tail (cl-ds.common.rrb:access-tail container))
                            (new-tail (if (null tail)
                                          (~> container
                                              cl-ds.common.rrb:read-element-type
                                              cl-ds.common.rrb:make-node-content)
                                          (copy-array tail))))
                       (setf (aref new-tail tail-size) new-bucket)
                       new-tail)
               :ownership-tag tag
               :tail-size (+ tail-size tail-change)
               :ownership-tag tag
               :size (cl-ds.common.rrb:access-size container)
               :shift (cl-ds.common.rrb:access-shift container))
         status))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:put!-function)
                                             (container transactional-rrb-vector)
                                             location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (debug 0) (safety 1)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
         (tag (cl-ds.common.abstract:make-ownership-tag))
         (tail-change 1)
         ((:values new-bucket status changed) (apply #'cl-ds.meta:make-bucket
                                                     operation
                                                     container
                                                     location
                                                     rest)))
    (unless changed
      (return-from cl-ds.meta:position-modification (values container status)))
    (if (eql tail-size +maximum-children-count+)
        (bind ((new-tail (~> container
                             cl-ds.common.rrb:read-element-type
                             cl-ds.common.rrb:make-node-content))
               ((:values new-root shift-increased)
                (cl-ds.common.rrb:insert-tail container
                                              tag
                                              #'cl-ds.common.rrb:transactional-copy-on-write
                                              (cl-ds.common.rrb:access-tail container))))
          (setf (aref new-tail 0) new-bucket)
          (setf (cl-ds.common.rrb:access-root container) new-root
                (cl-ds.common.rrb:access-tail container) new-tail
                (cl-ds.common.rrb:access-tail-size container) tail-change
                (cl-ds.common.rrb:access-size container) (+ +maximum-children-count+
                                                            (cl-ds.common.rrb:access-size container))
                (cl-ds.common.rrb:access-shift container) (if shift-increased
                                                              (1+ (cl-ds.common.rrb:access-shift container))
                                                              (cl-ds.common.rrb:access-shift container))))
        (setf (cl-ds.common.rrb:access-tail container)
              (let* ((tail (cl-ds.common.rrb:access-tail container))
                     (new-tail (or tail
                                   (~> container
                                       cl-ds.common.rrb:read-element-type
                                       cl-ds.common.rrb:make-node-content))))
                (setf (aref new-tail tail-size) new-bucket)
                new-tail)

              (cl-ds.common.rrb:access-tail-size container) (+ tail-size tail-change)))
    (values container status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:take-out-function)
                                             (container functional-rrb-vector)
                                             location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (safety 1) (debug 0)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
         (tag (cl-ds.common.abstract:make-ownership-tag))
         (result-status nil)
         (tail-change 0)
         ((:dflet shrink-bucket (bucket))
          (multiple-value-bind (bucket status changed)
              (apply #'cl-ds.meta:shrink-bucket
                     operation
                     container
                     bucket
                     location
                     rest)
            (setf result-status status)
            (unless changed
              (return-from cl-ds.meta:position-modification (values container status)))
            (when (cl-ds.meta:null-bucket-p bucket)
              (decf tail-change))
            bucket)))
    (values
     (if (zerop tail-size)
         (if (zerop (cl-ds.common.rrb:access-size container))
             (error 'cl-ds:empty-container :text "Can't take-out from empty container.")
             (bind (((:values new-root tail shift-decreased)
                     (cl-ds.common.rrb:remove-tail container))
                    (new-shift (if shift-decreased
                                   (1- (cl-ds.common.rrb:access-shift container))
                                   (cl-ds.common.rrb:access-shift container)))
                    (new-size (max 0 (- (cl-ds.common.rrb:access-size container)
                                        cl-ds.common.rrb:+maximum-children-count+)))
                    (new-tail (~> tail cl-ds.common.rrb:rrb-node-content))
                    (tail-size (if (null new-root)
                                   (cl-ds.common.rrb:access-size container)
                                   cl-ds.common.rrb:+maximum-children-count+))
                    (new-last-index (1- (+ tail-size tail-change)))
                    (new-bucket (~> new-tail
                                    (aref (1- cl-ds.common.rrb:+maximum-children-count+))
                                    shrink-bucket)))
               (cond ((and (cl-ds.meta:null-bucket-p new-bucket)
                           (zerop (+ tail-change tail-size)))
                      (setf new-tail nil))
                     ((~> new-bucket cl-ds.meta:null-bucket-p not)
                      (setf new-tail (copy-array new-tail)
                            (aref new-tail new-last-index) new-bucket))
                     (t (setf new-tail (copy-array new-tail)
                              (aref new-tail new-last-index) nil)))
               (assert (or new-root new-tail (zerop new-size)))
               (assert (<= 0 tail-size +maximum-children-count+))
               (make 'functional-rrb-vector
                     :root new-root
                     :tail new-tail
                     :ownership-tag tag
                     :tail-size (+ tail-size tail-change)
                     :size new-size
                     :shift new-shift)))
         (make 'functional-rrb-vector
               :root (cl-ds.common.rrb:access-root container)
               :tail (let* ((tail (cl-ds.common.rrb:access-tail container))
                            (new-bucket (shrink-bucket (aref tail (1- tail-size)))))
                       (unless (cl-ds.meta:null-bucket-p new-bucket)
                         (setf tail (copy-array tail)
                               (aref tail (+ tail-change tail-size)) new-bucket))
                       tail)
               :ownership-tag tag
               :tail-size (let ((s (+ tail-size tail-change)))
                            (assert (<= 0 s +maximum-children-count+))
                            s)
               :ownership-tag tag
               :size (cl-ds.common.rrb:access-size container)
               :shift (cl-ds.common.rrb:access-shift container)))
     result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (container transactional-rrb-vector)
                                             index &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (safety 1) (debug 0)))
  (bind ((tag (cl-ds.common.abstract:read-ownership-tag container))
         (shift (cl-ds.common.rrb:access-shift container))
         (size (cl-ds.common.rrb:access-size container))
         (root (cl-ds.common.rrb:access-root container))
         (result-status nil)
         ((:dflet change-bucket (bucket))
          (multiple-value-bind (node status changed)
              (apply #'cl-ds.meta:grow-bucket
                     operation
                     container
                     bucket
                     index
                     rest)
            (unless changed
              (return-from cl-ds.meta:position-modification
                (values container status)))
            (setf result-status status)
            node)))
    (unless (> (cl-ds:size container) index)
      (error 'cl-ds:argument-out-of-bounds
             :argument 'index
             :bounds (list 0 (cl-ds:size container))
             :value index
             :text "Index out of range."))
    (if (< index size)
        (bind (((:dflet cont (path indexes shift &aux (shift (1- shift))))
                (iterate
                  (with owned-depth = (iterate
                                        (for i from 0 below shift)
                                        (for node = (aref path i))
                                        (while (cl-ds.common.abstract:acquire-ownership node
                                                                                        tag))
                                        (finally (return i))))
                  (for i from shift downto 0)
                  (for position = (aref indexes i))
                  (for old-node = (aref path i))
                  (for node
                       initially (let* ((bucket (aref (cl-ds.common.rrb:rrb-node-content (aref path shift))
                                                      (aref index shift)))
                                        (next-value (change-bucket bucket))
                                        (content (if (eql shift owned-depth)
                                                     (aref path shift)
                                                     (copy-array (aref path shift)))))
                                   (setf (aref content (aref index shift)) next-value)
                                   (if (eql shift owned-depth)
                                       (aref path shift)
                                       (cl-ds.common.rrb:make-rrb-node :ownership-tag tag
                                                                       :content content)))
                       then (if (< i owned-depth)
                                (cl-ds.common.rrb:rrb-node-push! old-node
                                                                 position
                                                                 node)
                                (cl-ds.common.rrb:rrb-node-push-into-copy old-node
                                                                          position
                                                                          node
                                                                          tag)))
                  (finally (return node))))
               (new-root (if (zerop shift)
                             (cl-ds.common.rrb:rrb-node-push-into-copy
                              root
                              index
                              (change-bucket (~> root
                                                 cl-ds.common.rrb:rrb-node-content
                                                 (aref index)))
                              tag)
                             (cl-ds.common.rrb:descend-into-tree container index #'cont))))
          (setf (cl-ds.common.rrb:access-root container) new-root))
        (let* ((tail (~> container
                         cl-ds.common.rrb:access-tail))
               (offset (- index size)))
          (setf (aref tail offset)
                (change-bucket (aref tail offset)))))
    (values container result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (container functional-rrb-vector)
                                             index &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (safety 1) (debug 0)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
         (tag (cl-ds.common.abstract:make-ownership-tag))
         (shift (cl-ds.common.rrb:access-shift container))
         (size (cl-ds.common.rrb:access-size container))
         (root (cl-ds.common.rrb:access-root container))
         (result-status nil)
         (tail-change 0)
         ((:dflet change-bucket (bucket))
          (multiple-value-bind (node status changed)
              (apply #'cl-ds.meta:grow-bucket
                     operation
                     container
                     bucket
                     index
                     rest)
            (unless changed
              (return-from cl-ds.meta:position-modification
                (values container status)))
            (setf result-status status)
            node)))
    (unless (> (cl-ds:size container) index)
      (error 'cl-ds:argument-out-of-bounds
             :argument 'index
             :bounds (list 0 (cl-ds:size container))
             :value index
             :text "Index out of range."))
    (values
     (if (< index size)
         (bind (((:dflet cont (path indexes shift &aux (shift (1- shift))))
                 (iterate
                   (for i from shift downto 0)
                   (for position = (aref indexes i))
                   (for old-node = (aref path i))
                   (for node
                        initially (let* ((bucket (aref (cl-ds.common.rrb:rrb-node-content (aref path shift))
                                                       (aref index shift)))
                                         (next-value (change-bucket bucket))
                                         (content (copy-array (aref path shift))))
                                    (setf (aref content (aref index shift)) next-value)
                                    (cl-ds.common.rrb:make-rrb-node :ownership-tag tag
                                                                    :content content))
                        then (cl-ds.common.rrb:rrb-node-push-into-copy old-node
                                                                       position
                                                                       node
                                                                       tag))
                   (finally (return node))))
                (new-root (if (zerop shift)
                              (cl-ds.common.rrb:rrb-node-push-into-copy
                               root
                               index
                               (change-bucket (~> root
                                                  cl-ds.common.rrb:rrb-node-content
                                                  (aref index)))
                               tag)
                              (cl-ds.common.rrb:descend-into-tree container index #'cont)))
                ((:accessors (tail cl-ds.common.rrb:access-tail)
                             (shift cl-ds.common.rrb:access-shift))
                 container))
           (make 'functional-rrb-vector
                 :root new-root
                 :tail tail
                 :shift shift
                 :ownership-tag tag
                 :tail-size tail-size
                 :size size))
         (make 'functional-rrb-vector
               :root (cl-ds.common.rrb:access-root container)
               :tail (let* ((new-tail (~> container
                                          cl-ds.common.rrb:access-tail
                                          copy-array))
                            (offset (- index size)))
                       (setf (aref new-tail offset)
                             (change-bucket (aref new-tail offset)))
                       new-tail)
               :ownership-tag tag
               :tail-size tail-size
               :ownership-tag tag
               :size size
               :shift (cl-ds.common.rrb:access-shift container)))
     result-status)))


(defmethod cl-ds:empty-clone ((container rrb-vector))
  (make-instance (type-of container)
                 :root nil
                 :shift 0
                 :size 0
                 :tail-size 0
                 :tail nil))


(defmethod cl-ds:reset! ((obj mutable-rrb-vector))
  (bind (((:slots %root %shift %size %tail-size %tail) obj))
    (setf %root nil
          %shift 0
          %size 0
          %tail-size 0
          %tail nil)
    obj))


(defmethod cl-ds:become-mutable ((container functional-rrb-vector))
  (make 'mutable-rrb-vector
        :root (cl-ds.common.rrb:access-root container)
        :shift (cl-ds.common.rrb:access-shift container)
        :size (cl-ds.common.rrb:access-size container)
        :tail-size (cl-ds.common.rrb:access-tail-size container)
        :tail (copy-array (cl-ds.common.rrb:access-tail container))))


(defmethod cl-ds:become-functional ((container mutable-rrb-vector))
  (make 'functional-rrb-vector
        :root (cl-ds.common.rrb:access-root container)
        :shift (cl-ds.common.rrb:access-shift container)
        :size (cl-ds.common.rrb:access-size container)
        :tail-size (cl-ds.common.rrb:access-tail-size container)
        :tail (copy-array (cl-ds.common.rrb:access-tail container))))


(defmethod cl-ds:become-transactional ((container rrb-vector))
  (make 'transactional-rrb-vector
        :root (cl-ds.common.rrb:access-root container)
        :shift (cl-ds.common.rrb:access-shift container)
        :size (cl-ds.common.rrb:access-size container)
        :tail-size (cl-ds.common.rrb:access-tail-size container)
        :tail (and (cl-ds.common.rrb:access-tail container)
                   (copy-array (cl-ds.common.rrb:access-tail container)))))


(defmethod cl-ds:make-from-traversable ((class (eql 'mutable-rrb-vector))
                                        arguments
                                        traversable
                                        &rest more)
  (declare (ignore arguments))
  (let ((result (make 'mutable-rrb-vector)))
    (iterate
      (for tr in (cons traversable more))
      (cl-ds:across (lambda (x) (cl-ds:put! result x))
                    tr))
    result))


(defmethod cl-ds:make-from-traversable ((class (eql 'functional-rrb-vector))
                                        arguments
                                        traversable
                                        &rest more)
  (~> (apply #'cl-ds:make-from-traversable
             'mutable-rrb-vector
             arguments
             traversable
             more)
      cl-ds:become-functional))


(defmethod cl-ds:make-from-traversable ((class (eql 'transactional-rrb-vector))
                                        arguments
                                        traversable
                                        &rest more)
  (~> (apply #'cl-ds:make-from-traversable
             'mutable-rrb-vector
             arguments
             traversable
             more)
      cl-ds:become-transactional))


(defun make-functional-rrb-vector ()
  (make 'functional-rrb-vector))


(defun make-mutable-rrb-vector ()
  (make 'mutable-rrb-vector))


(defun make-transactional-rrb-vector ()
  (make 'transactional-rrb-vector))


(defmethod cl-ds:freeze! ((obj transactional-rrb-vector))
  (let ((result (call-next-method)))
    (unless result
      (cl-ds.common.abstract:enclose-finalizer obj))
    result))


(defmethod cl-ds:melt! ((obj transactional-rrb-vector))
  (let ((result (call-next-method)))
    (when result
      (trivial-garbage:cancel-finalization obj))
    result))


(defmethod cl-ds:transaction ((operation cl-ds.meta:put!-function)
                              (container transactional-rrb-vector)
                              &rest args)
  (bind ((result (cl-ds:become-transactional container)))
    (apply operation result args)
    (cl-ds:freeze! container)
    result))


(defmethod cl-ds:transaction ((operation cl-ds.meta:take-out!-function)
                              (container transactional-rrb-vector)
                              &rest args)
  (declare (ignore args))
  (bind ((result (cl-ds:become-transactional container)))
    (funcall operation result)
    (cl-ds:freeze! container)
    result))


(defmethod cl-ds:transaction ((operation cl-ds.meta:insert!-function)
                              (container transactional-rrb-vector)
                              &rest args)
  (bind ((result (cl-ds:become-transactional container)))
    (apply operation result args)
    (cl-ds:freeze! container)
    result))
