(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    co-occurence-table co-occurence-table-function

  (:range test-functions &key key)
  (:range test-functions &key (key #'identity))

  (%result %test-functions)

  ((&key test-functions &allow-other-keys)
   (setf %test-functions test-functions
         %result (make-array (make-list (length %test-functions)
                                        :initial-element 2)
                             :element-type 'non-negative-integer
                             :initial-element 0)))

  ((element)
   (let* ((address (map 'list
                        (lambda (fn)
                          (if (funcall fn element)
                              0 1))
                        %test-functions)))
     (apply #'(setf aref)
            (1+ (apply #'aref %result address))
            %result address)
     %result))

  (%result))
