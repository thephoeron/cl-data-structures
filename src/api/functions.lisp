(in-package #:cl-data-structures)


(defun traverse-multiple (function range &rest more)
  (let ((more (cons range more)))
    (map-into more #'whole-range more)
    (iterate
      (for data =
           (iterate
             (for r in more)
             (for (values data more) = (consume-front r))
             (unless more
               (return-from traverse-multiple nil))
             (collect data)))
      (apply function data))))


(defun iota-range (&key (from 0) to (by 1))
  (check-type to (or null integer))
  (check-type from integer)
  (check-type by integer)
  (cl-ds:xpr (:i from)
    (when (or (null to)
              (< i to))
      (send-recur i :i (+ by i)))))
