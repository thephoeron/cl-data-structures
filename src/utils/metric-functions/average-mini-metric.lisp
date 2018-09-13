(in-package #:cl-data-structures.utils.metric)


(defun average-mini-metric (fn a b)
  (declare (type vector a) (type vector b)
           (optimize (speed 3)))
  (ensure-functionf fn)
  (when (and (emptyp a) (emptyp b))
    (return-from average-mini-metric 0))
  (when (or (emptyp a) (emptyp b))
    (return-from average-mini-metric most-positive-fixnum))
  (bind ((left
          (iterate
            (for ea in-vector a)
            (for mini = (iterate
                          (for eb in-vector b)
                          (minimize (funcall fn ea eb))))
            (sum mini)))
         (right
          (iterate
            (for eb in-vector b)
            (for mini = (iterate
                          (for ea in-vector a)
                          (minimize (funcall fn eb ea))))
            (sum mini))))
    (/ (+ left right) (+ (length a) (length b)))))
