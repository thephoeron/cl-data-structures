(in-package #:cl-data-structures.sequences.rrb-vector)


(defclass functional-rrb-vector (cl-ds.common.rrb:rrb-container
                                 cl-ds.seqs:abstract-sequence
                                 cl-ds:functional)
  ())


(defmethod cl-ds:position-modification ((operation cl-ds:functional-put-function)
                                        (container functional-rrb-vector)
                                        location &key value)
  (declare (optimize (speed 3)))
  (let ((tail-size (cl-ds.common.rrb:access-tail-size container))
        (tag (cl-ds.common.abstract:make-ownership-tag)))
    (if (eql tail-size +maximum-children-count+)
        (bind ((new-tail (cl-ds.common.rrb:make-node-content))
               ((:values new-root shift-increased)
                (cl-ds.common.rrb:insert-tail container
                                              tag
                                              #'cl-ds.common.rrb:copy-on-write
                                              (cl-ds.common.rrb:access-tail container))))
          (setf (aref new-tail 0) (cl-ds:make-bucket operation container location
                                                     :value value))
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
                      (setf (aref new-tail tail-size)
                            (cl-ds:make-bucket operation
                                               container
                                               location
                                               :value value))
                      new-tail)
              :ownership-tag tag
              :tail-size (1+ tail-size)
              :ownership-tag tag
              :size (cl-ds.common.rrb:access-size container)
              :shift (cl-ds.common.rrb:access-shift container)))))


(defmethod cl-ds:position-modification ((operation cl-ds:take-out-function)
                                        (container functional-rrb-vector)
                                        location &key)
  (declare (optimize (speed 3)))
  (let ((tail-size (cl-ds.common.rrb:access-tail-size container))
        (tag (cl-ds.common.abstract:make-ownership-tag)))
    (if (zerop tail-size)
        (if (zerop (cl-ds.common.rrb:access-size container))
            (cl-ds.utils:todo)
            (bind ((zero-shift (zerop (cl-ds.common.rrb:access-shift container)))
                   ((:values new-root tail shift-decreased)
                    (if zero-shift
                        (values nil (cl-ds.common.rrb:access-root container) nil)
                        (cl-ds.common.rrb:remove-tail container
                                                      tag
                                                      #'cl-ds.common.rrb:copy-on-write-without-tail)))
                   (new-tail (and tail (~> tail cl-ds.common.rrb:rrb-node-content copy-array))))
              (unless (null new-tail)
                (setf (aref new-tail (1- cl-ds.common.rrb:+maximum-children-count+))
                      (cl-ds:shrink-bucket operation
                                           container
                                           (aref new-tail (1- cl-ds.common.rrb:+maximum-children-count+))
                                           location)))
              (make 'functional-rrb-vector
                    :root new-root
                    :tail new-tail
                    :ownership-tag tag
                    :tail-size (1- cl-ds.common.rrb:+maximum-children-count+)
                    :size (- (cl-ds.common.rrb:access-size container)
                             cl-ds.common.rrb:+maximum-children-count+)
                    :shift (if shift-decreased
                               (1- (cl-ds.common.rrb:access-shift container))
                               (cl-ds.common.rrb:access-shift container)))))
        (make 'functional-rrb-vector
              :root (cl-ds.common.rrb:access-root container)
              :tail (let* ((tail (cl-ds.common.rrb:access-tail container))
                           (new-tail (if (null tail)
                                         (cl-ds.common.rrb:make-node-content)
                                         (copy-array tail))))
                      (setf (aref new-tail (1- tail-size))
                            (cl-ds:shrink-bucket operation
                                                 container
                                                 (aref new-tail (1- tail-size))
                                                 location))
                      new-tail)
              :ownership-tag tag
              :tail-size (1- tail-size)
              :ownership-tag tag
              :size (cl-ds.common.rrb:access-size container)
              :shift (cl-ds.common.rrb:access-shift container)))))
