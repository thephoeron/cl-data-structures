(in-package #:cl-ds.utils)


(defun or* (&rest functions)
  (lambda (&rest rest)
    (iterate
      (for function in functions)
      (for result = (apply function rest))
      (when result
        (leave result)))))


(defun and* (&rest functions)
  (lambda (&rest rest)
    (iterate
      (for function in functions)
      (for result = (apply function rest))
      (always result)
      (finally (return result)))))


(defun cycle-over-address (dimensions &rest pinned)
  (bind ((address (make-list (length dimensions)
                             :initial-element 0))
         (cell nil)
         (finished nil)
         (current-dimension dimensions)
         (pinned-positions (batches pinned 2))
         (result nil))
    (when (oddp (length pinned))
      (error "Passed odd number of arguments as dimensions to pin"))
    (bind ((pinned (map 'vector
                        (lambda (x)
                          (lret ((result (nthcdr (first x) address)))
                            (setf (car result) (second x))))
                        pinned-positions)))
      (setf result (copy-list address))
      (lambda ()
        (if finished
            (shiftf result nil)
            (lret ((r result))
              (setf result (copy-list address))
              (iterate
                (for c on address)
                (for dim on current-dimension)
                (for pin = (find c pinned :test #'eq))
                (when pin
                  (next-iteration))
                (when (eq c cell)
                  (if (< (1+ (car c)) (car dim))
                      (progn
                        (setf cell c)
                        (incf (car c))
                        (leave address))
                      (progn
                        (iterate
                          (for sub on address)
                          (when (find sub pinned :test #'eq)
                            (next-iteration))
                          (until (eq sub c))
                          (setf (car sub) 0))
                        (setf (car cell) 0
                              cell (rest cell))
                        (next-iteration))))
                (setf cell c)
                (when (< (1+ (car c)) (car dim))
                  (incf (car c))
                  (leave address))
                (finally (setf finished t)))))))))
