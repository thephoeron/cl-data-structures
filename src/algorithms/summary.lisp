(in-package #:cl-data-structures.algorithms)


(defclass summary-aggregation-function (cl-ds.alg.meta:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass summary-result-range (hash-table-range)
  ())


(defstruct summary-aggregation-function-value
  arguments ids function-objects)


(defmethod cl-ds.alg.meta:make-state ((fn summary-aggregation-function)
                                      &rest all
                                      &key data)
  (declare (ignore all))
  (let ((arguments (map 'vector
                        #'funcall
                        (summary-aggregation-function-value-arguments data))))
    (vector data
            (map 'vector
                 (curry #'apply #'cl-ds.alg.meta:make-state)
                 (summary-aggregation-function-value-function-objects data)
                 arguments)
            (map 'vector (rcurry #'getf :key) arguments))))


(defmethod cl-ds.alg.meta:aggregate ((fn summary-aggregation-function)
                                     state
                                     element)
  (iterate
    (for sub in-vector (aref state 1))
    (for key in-vector (aref state 2))
    (for function in-vector
         (~> state (aref 0)
             summary-aggregation-function-value-function-objects))
    (cl-ds.alg.meta:aggregate function sub
                              (if (null key)
                                  element
                                  (funcall key element))))
  state)


(defmethod cl-ds.alg.meta:state-result ((fn summary-aggregation-function)
                                        state)
  (let ((table (make-hash-table)))
    (iterate
      (for sub in-vector (aref state 1))
      (for function in-vector
           (~> state (aref 0)
               summary-aggregation-function-value-function-objects))
      (for id in-vector
           (~> state (aref 0)
               summary-aggregation-function-value-ids))
      (setf (gethash id table)
            (cl-ds.alg.meta:state-result function sub)))
    (make-instance 'summary-result-range
                   :hash-table table
                   :keys (~> state
                             (aref 0)
                             summary-aggregation-function-value-ids)
                   :begin 0
                   :end (hash-table-count table))))


(defclass apply-aggregation-function-argument-interceptor ()
  ())


(defmethod cl-ds.alg.meta:apply-aggregation-function
    ((range apply-aggregation-function-argument-interceptor)
     (function cl-ds.alg.meta:aggregation-function)
     &rest all)
  all)


(def <interceptor> (make 'apply-aggregation-function-argument-interceptor))


(defun make-summary-aggregation-data (lambdas)
  (declare (optimize (debug 3)))
  (iterate
    (for (id symbol function callable) in lambdas)
    (check-type id symbol)
    (check-type symbol symbol)
    (unless (typep function 'cl-ds.alg.meta:aggregation-function)
      (error 'cl-ds:invalid-value
             :value function
             :format-control "~a is not an aggregation function!"
             :format-arguments (list symbol)))) ; TODO actually there is no reason why this must be this way. It just needs extra work.
  (make-summary-aggregation-function-value
   :function-objects (map 'vector #'third lambdas)
   :ids (map 'vector #'first lambdas)
   :arguments (map 'vector
                   (lambda (tuple)
                     (lambda ()
                       (~> tuple fourth (funcall <interceptor>))))
                   lambdas)))


(defgeneric %summary (range lambdas)
  (:generic-function-class summary-aggregation-function)
  (:method (range lambdas)
    (cl-ds.alg.meta:apply-range-function
     range #'%summary
     :key #'identity
     :data (make-summary-aggregation-data lambdas))))


(defmacro summary (range &body functions)
  (iterate
    (with !argument = (gensym))
    (for (id (function . body)) in (batches functions 2))
    (check-type id symbol)
    (check-type function symbol)
    (for _-p = (find :_ body :test 'eq))
    (for lambda-form = (if (null _-p)
                           `(lambda (,!argument)
                              (,function ,!argument ,@body))
                           `(lambda (,!argument)
                              (,function ,@(substitute !argument :_ body)))))
    (collect `(list ,id ',function #',function ,lambda-form)
      into form)
    (finally (return `(%summary ,range (list ,@form))))))
