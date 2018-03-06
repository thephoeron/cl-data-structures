(in-package #:cl-data-structures.common.egnat)


(defmacro front-body (range stack)
  `(if (null ,stack)
       (values nil nil)
       (bind (((node . index) (pop ,stack))
              (content (read-content node))
              (value.index
               (unless (null content)
                 (next-position ,range content index))))
         (if (null value.index)
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
               (push (cons node (cdr value.index)) ,stack)
               (values (car value.index) t))))))


(defmacro traverse-body (range stack function)
  `(iterate
     (until (null ,stack))
     (for (node . index) = (pop ,stack))
     (for content = (read-content node))
     (for value.index = (next-position ,range content index))
     (if (null value.index)
         (bind ((children (read-children node))
                (children-mask (select-children ,range children)))
           (iterate
             (for child in-vector children)
             (for present-bit in-vector children-mask)
             (for present = (eql 1 present-bit))
             (when present
               (push (cons child 0) stack))))
         (progn
           (push (cons node (cdr value.index)) ,stack)
           (funcall ,function (car value.index))))))


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


(defmethod cl-ds:reset! ((range egnat-range))
  (bind (((:slots %stack %initial-stack) range))
    (setf %stack %initial-stack))
  range)


(defmethod cl-ds:clone ((range egnat-range))
  (make-instance (type-of range)
                 :stack (access-stack range)
                 :initial-stack (access-stack range)
                 :container (read-container range)))


(defmethod cl-ds:clone ((range egnat-range-around))
  (make-instance (type-of range)
                 :stack (access-stack range)
                 :initial-stack (access-stack range)
                 :container (read-container range)
                 :near (read-near range)
                 :read-margin (read-margin range)))
