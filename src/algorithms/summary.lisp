(in-package #:cl-data-structures.algorithms)


(defclass summary-aggregation-function ()
  ((%arguments :type list
               :initarg :arguments
               :reader read-arguments
               :initform '())
   (%ids :type list
         :initarg :ids
         :reader read-ids
         :initform '())
   (%function-objects :type list
                      :initarg :function-objects
                      :reader read-function-objects
                      :initform '()))
  )

;; (setf (find-class 'summary-aggregation-function) nil)


(defmethod cl-ds.alg.meta:make-state ((fn summary-aggregation-function)
                                      &rest all)
  (declare (ignore all))
  (map 'vector (curry #'apply #'cl-ds.alg.meta:make-state)
       (read-function-objects fn)
       (read-arguments fn)))


(defmethod cl-ds.alg.meta:aggregate ((fn summary-aggregation-function)
                                     state
                                     element)
  (iterate
    (for sub in-vector state)
    (for function in (read-function-objects fn))
    (cl-ds.alg.meta:aggregate function sub element))
  state)


(defmethod cl-ds.alg.meta:state-result ((fn summary-aggregation-function)
                                        state)
  (let ((table (make-hash-table)))
    (iterate
      (for sub in-vector state)
      (for function in (read-function-objects fn))
      (for id in (read-ids fn))
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


(defun make-summary-aggregation-function (lambdas)
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
             :text (format nil "~a is a multi-aggregation-function. Only single stage aggregation functions are supposted by summary." symbol)))) ; TODO actually there is no reason why this must be this way. It just needs extra work.
  (make 'summary-aggregation-function
        :function-objects (mapcar #'third lambdas)
        :ids (mapcar #'first lambdas)
        :arguments (mapcar (compose (rcurry #'funcall <interceptor>)
                                    #'fourth)
                           lambdas)))


(defun %summary (range lambdas)
  (cl-ds.alg.meta:apply-aggregation-function
   range (make-summary-aggregation-function lambdas)
   :key #'identity))


(defmacro summary (range &body functions)
  (with-gensyms (!argument)
    `(%summary
      ,range
      (list ,@(iterate
                (for (id (function . body)) in (batches functions 2))
                (collect `(list ,id ',function #',function
                                (lambda (,!argument)
                                  (,function ,!argument ,@body)))))))))
