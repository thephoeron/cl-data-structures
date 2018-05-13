(in-package #:cl-data-structures.math)


(defmacro bootstrap (lambda-list vector-symbol sample-symbol value-form samples-count sample-size)
  (with-gensyms (!results)
    (once-only (samples-count sample-size)
      `(lambda ,lambda-list
         (iterate
           (with ,!results = (vect))
           (repeat ,samples-count)
           (for ,sample-symbol = (cl-ds.utils:draw-sample-vector ,vector-symbol ,sample-size))
           (vector-push-extend ,value-form results)
           (finally (return (statistical-summary ,!results))))))))
