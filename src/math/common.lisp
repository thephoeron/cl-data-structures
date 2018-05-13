(in-package #:cl-data-structures.math)


(defmacro bootstrap (lambda-list vector-symbol sample-symbol value-form samples-count sample-size)
  (with-gensyms (!results !i)
    (once-only (samples-count sample-size)
      `(lambda ,lambda-list
         (iterate
           (with ,!results = (make-array ,sample-size :element-type 'real))
           (for ,!i from 0 below ,samples-count)
           (for ,sample-symbol = (cl-ds.utils:draw-sample-vector ,vector-symbol ,sample-size))
           (setf (aref ,!results ,!i) ,value-form)
           (finally (return (statistical-summary ,!results))))))))
