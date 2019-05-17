(cl:in-package #:cl-ds.rf)


(defmethod make-submodel-with-model ((main-model random-forest-classifier)
                                     data
                                     arguments)
  (make-submodel (submodel-class main-model) data arguments))


(defmethod make-node ((main-model random-forest-classifier)
                      data arguments)
  (bind ((split-attempts (split-attempts main-model))
         (hash-table (make-hash-table))
         ((:labels impl (data depth))
          (when (zerop depth)
            (return-from impl nil))
          (clrhash hash-table)
          (bind ((submodel (lret ((r (make-submodel-with-model main-model
                                                               data
                                                               arguments)))
                             (train r data)))
                 (submodel-predictions (~> (predict main-model data)
                                           cl-ds.alg:to-vector))
                 (data-size (length data))
                 (grouped-predictions (cl-ds.alg:group-by
                                       submodel-predictions
                                       :key #'second))
                 (gini-impurity (~> (cl-ds.alg:summary
                                        grouped-predictions
                                      :count (cl-ds.alg:count-elements)
                                      :gini (cl-ds.math:gini-impurity
                                             submodel-predictions
                                             :hash-table hash-table
                                             :key #'cadar))
                                    (cl-ds.alg:accumulate
                                     #'+
                                     :initial-value 0.0
                                     :key (lambda (x)
                                            (* (cl-ds:at x :count)
                                               (/ (cl-ds:at x :gini)
                                                  data-size)))))))

            )))))
