(in-package #:cl-data-structures.algorithms)


(defclass state-extractor ()
  ())


(defstruct summary-state key effective-state fn)


(defmethod apply-aggregation-function ((range state-extractor)
                                       (function aggregation-function)
                                       &rest args
                                       &key (key #'identity) &allow-other-keys)
  (values
   (apply #'make-state function args)
   key))


(let ((extractor (make 'state-extractor)))
  (cl-ds.alg.meta:define-aggregation-function
      summary summary-function

    (:range forms &key key)
    (:range forms &key (key #'identity))

    (%table)

    ((&key forms)
     (setf %table (make-hash-table))
     (iterate
       (for form in forms)
       (for label = (cl-ds:at form :name))
       (for (fn-obj . arguments) = (cl-ds:at form :fn))
       (for replaced = (substitute extractor :range arguments))
       (for (values state key-function) = (apply fn-obj replaced))
       (setf (gethash label %table) (make-summary-state :key key-function
                                                        :effective-state state
                                                        :fn fn-obj))))

    ((element)
     (maphash (lambda (key value)
                (declare (ignore key))
                (with-accessors ((key summary-state-key)
                                 (state summary-state-effective-state)
                                 (fn summary-state-fn)) value
                  (aggregate fn state (funcall key element))))
              %table))

    ((maphash (lambda (key value)
                (setf (gethash key %table)
                      (state-result (summary-state-fn value)
                                    (summary-state-effective-state value))))
              %table)
     (make-hash-table-range %table))))


(cl-ds:define-validation-for-fields (summary-function
                                     (:name :fn :key :arguments))
  (:name :optional nil)
  (:key :optional t
        :default #'identity)
  (:fn :optional nil
       :type 'list))
