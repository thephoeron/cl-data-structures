(in-package #:cl-data-structures.algorithms)


(defclass summary-aggregation-function (cl-ds.alg.meta:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass summary-result-range (hash-table-range)
  ())


(cl-ds.alg.meta:define-aggregation-function
    %summary %summary-function

    (:range ids constructors)
    (:range ids constructors)

    (%aggregators %ids)

    ((setf %aggregators (mapcar #'funcall constructors)
           %ids ids))

    ((element)
     (iterate
       (for aggregator in %aggregators)
       (cl-ds.alg.meta:pass-to-aggregation aggregator element)))

    ((let ((result (make-hash-table :test 'eq :size (length %aggregators))))
       (iterate
         (for aggregator in %aggregators)
         (for id in %ids)
         (setf (gethash id result) (cl-ds.alg.meta:extract-result aggregator)))
       (make-instance 'summary-result-range
                      :hash-table result
                      :keys (coerce %ids 'vector)
                      :begin 0
                      :end (hash-table-count result)))))


(defmacro summary (range &body functions)
  (once-only (range)
    (iterate
      (for (id (function . body)) in (batches functions 2))
      (check-type id symbol)
      (check-type function symbol)
      (for aggregator = `(cl-ds.alg.meta:aggregator-constructor
                          '()
                          nil
                          (function ,function)
                          (list ,range ,@body)))
      (collect aggregator into forms)
      (collect id into ids)
      (finally (return `(%summary ,range ',ids (list ,@forms)))))))
