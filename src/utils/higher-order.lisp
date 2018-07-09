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


(defun cycle-over-address (dimensions)
  (bind ((address (make-list (length dimensions)
                             :initial-element 0))
         (cell address)
         (finished nil)
         (current-dimension dimensions))
    (setf (first address) -1)
    (lambda ()
      (unless finished
        (iterate
          (for c on address)
          (for dim on current-dimension)
          (when (eq c cell)
            (if (< (1+ (car c)) (car dim))
                (progn
                  (incf (car c))
                  (leave address))
                (progn
                  (iterate
                    (for sub on address)
                    (until (eq sub c))
                    (setf (car sub) 0))
                  (setf (car cell) 0
                        cell (rest cell))
                  (next-iteration))))
          (setf cell c)
          (when (< (1+ (car c)) (car dim))
            (incf (car c))
            (leave address))
          (finally (setf finished t)))))))
