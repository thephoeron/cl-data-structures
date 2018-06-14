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
