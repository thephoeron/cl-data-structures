(in-package #:cl-data-structures.sequences.rrb-vector)


(defclass functional-rrb-vector (cl-ds.common.rrb:rrb-container
                                 cl-ds.seqs:functional-sequence)
  ())


(defclass mutable-rrb-vector (cl-ds.common.rrb:rrb-container
                              cl-ds.seqs:mutable-sequence)
  ())


(defmethod cl-ds:position-modification ((operation cl-ds:take-out-function)
                                        (container mutable-rrb-vector)
                                        location &rest rest &key &allow-other-keys)
  (bind ((result-status nil)
         (tail-change 0)
         ((:dflet shrink-bucket (bucket))
          (multiple-value-bind (bucket status changed)
              (apply #'cl-ds:shrink-bucket!
                     operation container
                     bucket location
                     rest)
            (setf result-status status)
            (unless changed
              (return-from cl-ds:position-modification (values container status)))
            (when (null bucket)
              (decf tail-change))
            bucket))
         (root (cl-ds.common.rrb:access-root container))
         (shift (cl-ds.common.rrb:access-shift container))
         (tail-size (cl-ds.common.rrb:access-tail-size container))
         (tail (cl-ds.common.rrb:access-tail container)))
    (cond ((null tail)
           (if (null root)
               (error 'cl-ds:empty-container :text "Can't take-out from empty container.")
               (bind ((zero-shift (zerop shift))
                      ((:values new-root new-tail shift-decreased)
                       (if zero-shift
                           (values nil (cl-ds.common.rrb:access-root container) nil)
                           (cl-ds.common.rrb:remove-tail container)))
                      ((:values new-bucket status changed) (and new-tail (shrink-bucket (aref new-root (1- cl-ds.common.rrb:+maximum-children-count+))))))
                 (setf (cl-ds.common.rrb:access-tail container) new-tail)
                 (unless (null new-tail)
                   (unless (null new-bucket)
                     (setf (aref new-tail (1- cl-ds.common.rrb:+maximum-children-count+))
                           (shrink-bucket (aref new-tail (1- cl-ds.common.rrb:+maximum-children-count+)))))
                   (setf (cl-ds.common.rrb:access-tail-size container)
                         (+ cl-ds.common.rrb:+maximum-children-count+ tail-change))))))
          ((eql 1 tail-size)
           (setf (cl-ds.common.rrb:access-tail-size container) 0
                 (cl-ds.common.rrb:access-tail container) nil))
          (t
           (setf (aref tail (decf (cl-ds.common.rrb:access-tail-size container)))
                 nil)))
    container))


(defmethod cl-ds:position-modification ((operation cl-ds:functional-put-function)
                                        (container functional-rrb-vector)
                                        location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3)))
  (block nil
    (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
           (tag (cl-ds.common.abstract:make-ownership-tag))
           ((:values new-bucket status changed) (apply #'cl-ds:make-bucket
                                                       operation
                                                       container
                                                       location
                                                       rest)))
      (unless changed
        (return container))
      (if (eql tail-size +maximum-children-count+)
          (bind ((new-tail (cl-ds.common.rrb:make-node-content))
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
                  :tail-size 1
                  :size (+ +maximum-children-count+
                           (cl-ds.common.rrb:access-size container))
                  :tail new-tail
                  :shift (if shift-increased
                             (1+ (cl-ds.common.rrb:access-shift container))
                             (cl-ds.common.rrb:access-shift container))))
          (make 'functional-rrb-vector
                :root (cl-ds.common.rrb:access-root container)
                :tail (let* ((tail (cl-ds.common.rrb:access-tail container))
                             (new-tail (if (null tail)
                                           (cl-ds.common.rrb:make-node-content)
                                           (copy-array tail))))
                        (setf (aref new-tail tail-size) new-bucket)
                        new-tail)
                :ownership-tag tag
                :tail-size (1+ tail-size)
                :ownership-tag tag
                :size (cl-ds.common.rrb:access-size container)
                :shift (cl-ds.common.rrb:access-shift container))))))


(defmethod cl-ds:position-modification ((operation cl-ds:take-out-function)
                                        (container functional-rrb-vector)
                                        location &rest rest &key &allow-other-keys)
  (declare (optimize (debug 3)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
         (tag (cl-ds.common.abstract:make-ownership-tag))
         (result-status nil)
         (tail-change 0)
         ((:dflet shrink-bucket (bucket))
          (multiple-value-bind (bucket status changed)
              (apply #'cl-ds:shrink-bucket
                     operation
                     container
                     bucket
                     location
                     rest)
            (setf result-status status)
            (unless changed
              (return-from cl-ds:position-modification (values container status)))
            (when (null bucket)
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
                    (new-bucket (~> new-tail
                                    (aref (1- cl-ds.common.rrb:+maximum-children-count+))
                                    shrink-bucket)))
               (cond ((and (null new-bucket) (zerop (+ tail-change tail-size)))
                      (setf new-tail nil))
                     (new-bucket
                      (setf new-tail (copy-array new-tail)
                            (aref new-tail (+ tail-change tail-size)) new-bucket)))
               (assert (or new-root new-tail (zerop new-size)))
               (assert (<= 0 tail-size +maximum-children-count+))
               (make 'functional-rrb-vector
                     :root new-root
                     :tail new-tail
                     :ownership-tag tag
                     :tail-size (if (null new-tail)
                                    0
                                    (+ tail-size tail-change))
                     :size new-size
                     :shift (if shift-decreased
                                (1- (cl-ds.common.rrb:access-shift container))
                                (cl-ds.common.rrb:access-shift container)))))
         (make 'functional-rrb-vector
               :root (cl-ds.common.rrb:access-root container)
               :tail (let* ((tail (cl-ds.common.rrb:access-tail container))
                            (new-bucket (shrink-bucket  (aref tail (1- tail-size)))))
                       (unless (null new-bucket)
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


(defmethod cl-ds:position-modification ((operation cl-ds:grow-function)
                                        (container functional-rrb-vector)
                                        index &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size container))
         (tag (cl-ds.common.abstract:make-ownership-tag))
         (shift (cl-ds.common.rrb:access-shift container))
         (size (cl-ds.common.rrb:access-size container))
         (root (cl-ds.common.rrb:access-root container))
         (result-status nil)
         ((:dflet change-bucket (bucket))
          (multiple-value-bind (node status changed)
              (apply #'cl-ds:grow-bucket
                     operation
                     container
                     bucket
                     index
                     rest)
            (unless changed
              (return-from cl-ds:position-modification
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
               :tail (let* ((new-tail (~> container cl-ds.common.rrb:access-tail copy-array))
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
