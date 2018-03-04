(in-package #:cl-data-structures.common.egnat)


(defmethod cl-ds:consume-front ((range egnat-range))
  (bind ((stack (access-stack range)))
    (if (null stack)
        (values nil nil)
        (bind ((node.index (pop stack))
               (bucket (read-content node.index))
               (value.next-node.index
                (unless (null bucket)
                  (next-bucket-position range bucket (cdr node.index)))))
          (if (null value.next-node.index)
              (bind ((children (read-children (car node.index)))
                     (children-mask (select-children range children)))
                (iterate
                  (for child in-vector children)
                  (for present-bit in-vector children-mask)
                  (for present = (eql 1 present-bit))
                  (when present
                    (push (cons child 0) stack))
                  (finally (progn (setf (access-stack range) stack)
                                  (cl-ds:consume-front range)))))
              (progn
                (push stack (cdr value.next-node.index))
                (setf (access-stack range) stack)
                (values (car value.next-node.index) t)))))))
