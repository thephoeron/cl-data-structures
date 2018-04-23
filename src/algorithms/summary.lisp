(in-package #:cl-data-structures.algorithms)


(defclass summary-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric summary (range &rest forms)
  (:generic-function-class summary-function)
  (:method (range &rest forms)
    (apply-aggregation-function range #'summary :forms forms :key #'identity)))


(defclass state-extractor ()
  ())


(defmethod apply-aggregation-function ((range state-extractor) (function aggregation-function)
                                       &rest args &key (key #'identity) &allow-other-keys)
  (values
   (apply #'make-state function args)
   key))


(defstruct summary-state key effective-state fn)


(let ((extractor (make 'state-extractor)))
  (defmethod make-state ((function summary-function) &key forms &allow-other-keys)
    (let ((result (make-hash-table)))
      (iterate
        (for form in forms)
        (for (label fn . args) = form)
        (for fn-obj = (if (functionp fn) fn (symbol-function fn)))
        (for (values state key-function) = (apply fn-obj (substitute extractor :range args)))
        (setf (gethash label result)
              (make-summary-state :key key-function :effective-state state :fn fn-obj)))
      result)))


(defmethod aggregate ((function summary-function)
                      state
                      element)
  (maphash (lambda (key value)
             (declare (ignore key))
             (with-accessors ((key summary-state-key)
                              (state summary-state-effective-state)
                              (fn summary-state-fn)) value
               (aggregate fn state (funcall key element))))
           state))


(defmethod state-result ((function summary-function)
                         state)
  (maphash (lambda (key value)
             (setf (gethash key state)
                   (state-result (summary-state-fn value)
                                 (summary-state-effective-state value))))
           state)
  (make-hash-table-range state))
