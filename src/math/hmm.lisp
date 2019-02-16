(in-package #:cl-data-structures.math)


(defun check-probability-in-rows (argument table)
  (iterate
    (for i from 0 below (array-dimension table 0))
    (for sum =
         (iterate
           (for j from 0 below (array-dimension table 1))
           (for value = (aref table i j))
           (when (negative-float-p value)
             (error 'cl-ds:out-of-bounds
                    :bounds '(0.0 1.0)
                    :value value
                    :text "Probability cannot be negative."))
           (sum value)))
    (when (> (abs (- sum 1.0)) single-float-epsilon)
      (error 'cl-ds:incompatible-argument
             :argument argument
             :text "Probability in the row does not sum to 1.0"))))


(defun random-float ()
  (coerce (/ (random most-positive-fixnum)
             most-positive-fixnum)
          'double-float))


(defun sample-position (table row)
  (declare (type (array float (* *)) table)
           (type fixnum row))
  (iterate
    (with random-value = (random-float))
    (with chance-sum = 0.0)
    (for i from 0 below (array-dimension table 1))
    (for next-chance-sum = (+ chance-sum
                              (aref table row i)))
    (finding i such-that (and (< chance-sum random-value)
                              (<= random-value next-chance-sum)))
    (setf chance-sum next-chance-sum)))


(defun hidden-markov-model-generator (observations
                                      trans-table
                                      observation-table
                                      &key (initial-state 0))
  (check-type observations vector)
  (check-type trans-table (array float (* *)))
  (check-type observation-table (array float (* *)))
  (check-type initial-state non-negative-fixnum)
  (bind ((dim1 (array-dimensions trans-table))
         (dim2 (array-dimensions observation-table))
         (observations-count (length observations)))
    (unless (= (first dim1) (first dim2) (second dim1))
      (error 'cl-ds:incompatible-argument
             :argument '(trans-table observation-table)
             :text "Inconsistent number of model states in the input arrays."))
    (unless (= (second dim2) observations-count)
      (error 'cl-ds:incompatible-argument
             :argument '(observations observation-table)
             :text "Inconsistent number of model observations in the input arrays."))
    (check-probability-in-rows 'trans-table trans-table)
    (check-probability-in-rows 'observation-table observation-table)
    (when (> initial-state (first dim1))
      (error 'cl-ds:argument-out-of-bounds
             :argument 'initial-state
             :value initial-state
             :bounds `(0 ,(first dim1))
             :text "Initial state does not exist in the states table."))
    (cl-ds:xpr (:state initial-state)
      (cl-ds:send-recur (~>> (sample-position observation-table state)
                             (aref observations))
                        :state (sample-position trans-table state)))))
