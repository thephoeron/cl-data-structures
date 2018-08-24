(in-package #:cl-data-structures.utils.metric)


(defun euclid-metric (a b)
  (declare (type vector a b))
  (assert (eql (length a) (length b)))
  (when (zerop (length a))
    (return-from euclid-metric 0.0))
  (iterate
    (for ea in-vector a)
    (for eb in-vector b)
    (sum (expt (- ea eb) 2) into sum)
    (finally (return (sqrt sum)))))
