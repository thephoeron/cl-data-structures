(cl:in-package #:cl-ds.rf)


(defmethod make-submodel-with-model ((main-model random-forest-classifier)
                                     data
                                     arguments)
  (make-submodel (submodel-class main-model) data arguments))


(defmethod make-node ((main-model random-forest-classifier)
                      data arguments)
  (bind (((:labels impl (data depth))
          (when (zerop depth)
            (return-from impl nil))
          (let ((submodel (make-submodel-with-model main-model
                                                    data
                                                    arguments)))
            (train submodel data)
            )))
    ))
