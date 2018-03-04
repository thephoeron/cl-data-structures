(in-package #:cl-data-structures.common.egnat)


(defmacro front-body (range stack)
  `(if (null ,stack)
       (values nil nil)
       (bind (((node . index) (pop ,stack))
              (bucket (read-content node))
              (value.next-node.index
               (unless (null bucket)
                 (next-bucket-position ,range bucket index))))
         (if (null value.next-node.index)
             (bind ((children (read-children node))
                    (children-mask (select-children ,range children)))
               (iterate
                 (for child in-vector children)
                 (for present-bit in-vector children-mask)
                 (for present = (eql 1 present-bit))
                 (when present
                   (push (cons child 0) stack))
                 (finally (return (cl-ds:consume-front ,range)))))
             (progn
               (push ,stack (cdr value.next-node.index))
               (values (car value.next-node.index) t))))))


(defmacro traverse-body (range stack function)
  `(iterate
     (until (null ,stack))
     (for (node . index) = (pop ,stack))
     (for bucket = (read-content node))
     (for next = (next-bucket-position ,range bucket index))
     (if (null next)
         (bind ((children (read-children node))
                (children-mask (select-children ,range children)))
           (iterate
             (for child in-vector children)
             (for present-bit in-vector children-mask)
             (for present = (eql 1 present-bit))
             (when present
               (push (cons child 0) stack))))
         (progn
           (push ,stack (cdr next))
           (funcall ,function (car next))))))


(cl-ds.common:defmethod-with-stack
    (cl-ds:consume-front
     ((range egnat-range))
     stack (access-stack range))
  (front-body range stack))


(cl-ds.common:defmethod-with-peek-stack
    (cl-ds:peek-front
     ((range egnat-range))
     stack (access-stack range))
  (front-body range stack))


(cl-ds.common:defmethod-with-peek-stack
    (cl-ds:across
     (function (range egnat-range))
     stack (access-stack range))
  (traverse-body range stack function))


(cl-ds.common:defmethod-with-stack
    (cl-ds:traverse
     (function (range egnat-range))
     stack (access-stack range))
  (traverse-body range stack function))
