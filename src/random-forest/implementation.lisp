(cl:in-package #:cl-ds.rf)


(defmethod make-submodel-with-model ((main-model random-forest-classifier)
                                     data
                                     arguments)
  (make-submodel (submodel-class main-model) data arguments))


(defmethod make-node ((main-model random-forest-classifier)
                      data arguments)
  (bind ((split-attempts (split-attempts main-model))
         ((:labels node (data depth))
          (when (zerop depth)
            (return-from node nil))
          (bind ((submodel (lret ((r (make-submodel-with-model main-model
                                                               data
                                                               arguments)))
                             (train r data)))
                 (submodel-predictions (predict main-model data))
                 (data-size (length data))
                 (grouped-predictions (cl-ds.alg:group-by
                                       submodel-predictions
                                       :key #'second))
                 (summary (cl-ds.alg:summary grouped-predictions
                            :count (cl-ds.alg:count-elements)
                            :content (cl-ds.alg:to-vector :key #'first)
                            :gini (cl-ds.math:gini-impurity
                                   submodel-predictions
                                   :key #'cadar)))
                 (classes-count (cl-ds:size summary))
                 (gini-impurity (cl-ds.alg:accumulate
                                 summary
                                 #'+
                                 :initial-value 0.0
                                 :key (lambda (group &aux (x (cdr group)))
                                        (* (cl-ds:at x :count)
                                           (/ (cl-ds:at x :gini)
                                              data-size))))))

            )))))
