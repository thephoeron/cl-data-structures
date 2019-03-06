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
  (if (or (null to) (< from to))
      (progn
        (unless (positive-integer-p by)
          (error 'argument-out-of-bounds
                 :format-control "BY must be positive because TO is larger then FROM."
                 :argument 'by
                 :bounds '(> 0)
                 :value by))
        (cl-ds:xpr (:i from)
          (when (or (null to)
                    (< i to))
            (send-recur i :i (+ by i)))))
      (progn
        (unless (negative-integer-p by)
          (error 'argument-out-of-bounds
                 :format-control "BY must be negative because TO is smaller then FROM."
                 :argument 'by
                 :bounds '(< 0)
                 :value by))
        (cl-ds:xpr (:i from)
          (when (or (null to)
                    (> i to))
            (send-recur i :i (+ by i)))))))


(defun modulo-range (size &key (start 0) (by 1))
  (check-type size positive-integer)
  (check-type start non-negative-integer)
  (check-type by positive-integer)
  (cl-ds:xpr (:i (rem start size))
    (cl-ds:send-recur i :i (rem (+ i by) size))))
