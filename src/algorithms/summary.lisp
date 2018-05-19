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


(defstruct summary-field name function arguments)


(defun summary-field (field)
  (make-summary-field :name (cl-ds:name field)
                      :function (if (functionp (cl-ds:at field :fn))
                                    (cl-ds:at field :fn)
                                    (symbol-function (cl-ds:at field :fn)))
                      :arguments (cl-ds:at field :args)))


(let ((extractor (make 'state-extractor)))
  (cl-ds.alg.meta:define-aggregation-function
      summary summary-function

    (:range &rest forms)
    (:range &rest forms)

    (%table)

    ((&key forms)
     (setf %table (make-hash-table))
     (iterate
       (for form in (mapcar #'summary-field forms))
       (for label = (summary-field-name form))
       (for fn-obj = (summary-field-function form))
       (for args = (summary-field-arguments form))
       (for (values state key-function) = (apply fn-obj
                                                 (substitute extractor
                                                             :range args)))
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
