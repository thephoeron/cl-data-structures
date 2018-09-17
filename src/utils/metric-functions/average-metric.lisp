(in-package #:cl-data-structures.utils.metric)


(defun average-metric (fn test a b &key (key #'identity))
  (declare (type vector a) (type vector b)
           (optimize (speed 3)))
  (ensure-functionf fn)
  (when (and (emptyp a) (emptyp b))
    (return-from average-metric 0))
  (when (or (emptyp a) (emptyp b))
    (return-from average-metric most-positive-fixnum))
  (bind ((union (~> (concatenate 'vector a b)
                    (delete-duplicates :test test :key key)))
         (left
          (iterate
            (with val = (/ 1 (* (length union) (length a))))
            (for ea in-vector a)
            (when (null (position ea b :test test :key key))
              (next-iteration))
            (for eea = (funcall key ea))
            (sum (iterate
                   (for eb in-vector b)
                   (for eeb = (funcall key eb))
                   (sum (funcall fn eea eeb)))
                 into result)
            (finally (return (* val result)))))
         (right
          (iterate
            (with val = (/ 1 (* (length union) (length b))))
            (for eb in-vector a)
            (when (null (position eb a :test test :key key))
              (next-iteration))
            (for eeb = (funcall key eb))
            (sum (iterate
                   (for ea in-vector a)
                   (for eea = (funcall key ea))
                   (sum (funcall fn eea eeb)))
                 into result)
            (finally (return (* val result))))))
    (+ left right)))
