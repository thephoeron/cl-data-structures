(in-package #:cl-data-structures.algorithms)


(defclass summary-aggregation-function (cl-ds.alg.meta:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(def <summary-aggregation> (make 'summary-aggregation-function))


(defstruct summary-aggregation-function-value
  arguments ids function-objects)


(defmethod cl-ds.alg.meta:make-state ((fn summary-aggregation-function)
                                      &rest all
                                      &key data)
  (declare (ignore all))
  (let ((arguments (summary-aggregation-function-value-arguments data)))
    (list data
          (map 'vector (curry #'apply #'cl-ds.alg.meta:make-state)
               (summary-aggregation-function-value-function-objects data)
               arguments)
          (mapcar (lambda (x) (getf x :key)) arguments))))


(defmethod cl-ds.alg.meta:aggregate ((fn summary-aggregation-function)
                                     state
                                     element)
  (iterate
    (for sub in-vector (second state))
    (for key in (third state))
    (for function in
         (~> state first
             summary-aggregation-function-value-function-objects))
    (cl-ds.alg.meta:aggregate function sub (funcall key element)))
  state)


(defmethod cl-ds.alg.meta:state-result ((fn summary-aggregation-function)
                                        state)
  (let ((table (make-hash-table)))
    (iterate
      (for sub in-vector (second state))
      (for function in
           (~> state first
               summary-aggregation-function-value-function-objects))
      (for id in
           (~> state first
               summary-aggregation-function-value-ids))
      (setf (gethash id table)
            (cl-ds.alg.meta:state-result function sub)))
    (make-hash-table-range table)))


(defclass apply-aggregation-function-argument-interceptor ()
  ())


(defmethod cl-ds.alg.meta:apply-aggregation-function
    ((range apply-aggregation-function-argument-interceptor)
     (function cl-ds.alg.meta:aggregation-function)
     &rest all)
  all)


(def <interceptor> (make 'apply-aggregation-function-argument-interceptor))


(defun make-summary-aggregation-data (lambdas)
  (iterate
    (for (id symbol function callable) in lambdas)
    (check-type id symbol)
    (check-type symbol symbol)
    (unless (typep function 'cl-ds.alg.meta:aggregation-function)
      (error 'cl-ds:invalid-argument
             :argument symbol
             :text (format nil "~a is not an aggregation function!" symbol)))
    (when (typep function 'cl-ds.alg.meta:multi-aggregation-function)
      (error 'cl-ds:invalid-argument
             :argument symbol
             :text (format nil "~a is a multi-aggregation-function. Only single stage aggregation functions are supposted by summary."
                           symbol)))) ; TODO actually there is no reason why this must be this way. It just needs extra work.
  (make-summary-aggregation-function-value
   :function-objects (mapcar #'third lambdas)
   :ids (mapcar #'first lambdas)
   :arguments (mapcar (compose (rcurry #'funcall <interceptor>)
                               #'fourth)
                      lambdas)))


(defun %summary (range lambdas)
  (cl-ds.alg.meta:apply-range-function
   range <summary-aggregation>
   :key #'identity
   :data (make-summary-aggregation-data lambdas)))


(defmacro summary (range &body functions)
  (iterate
    (with !argument = (gensym))
    (for (id (function . body)) in (batches functions 2))
    (for _-p = (find "_" body
                     :key (cl-ds.utils:and* #'symbolp #'symbol-name)
                     :test 'equal))
    (for lambda-form = (if (null _-p)
                           `(lambda (,!argument)
                              (,function ,!argument ,@body))
                           `(lambda (,_-p)
                              (,function ,@body))))
    (collect `(list ,id ',function #',function ,lambda-form)
      into form)
    (finally (return `(%summary ,range (list ,@form))))))
