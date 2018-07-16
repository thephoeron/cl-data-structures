(in-package #:cl-data-structures.utils)


(defun cartesian (sequence-of-sequences result-callback)
  (unless (some #'emptyp sequence-of-sequences)
    (let* ((lengths (map 'list #'length sequence-of-sequences))
           (generator (cycle-over-address lengths)))
      (iterate
        (for adr = (funcall generator))
        (while adr)
        (apply result-callback
               (map 'list #'elt sequence-of-sequences adr))))))


(defun cartesian-table (sequence-of-sequences fn)
  (unless (some #'emptyp sequence-of-sequences)
    (let* ((lengths (map 'list #'length sequence-of-sequences))
           (generator (cycle-over-address lengths))
           (result-table (make-array lengths)))
      (iterate
        (for adr = (funcall generator))
        (while adr)
        (apply #'(setf aref)
               (apply fn (map 'list #'elt sequence-of-sequences adr))
               result-table
               adr))
      result-table)))


