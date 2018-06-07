(in-package #:cl-data-structures.data-frame)


(defun column-at (number)
  (rcurry #'column number))


(defun row-at (number)
  (rcurry #'row number))


(defun stack (dimension key &rest data))


(defun range-stack (dimension range &key (key #'identity)))


(defun row (data number)
  (plane data nil number))


(defun column (data number)
  (plane data number nil))


(defun at-cell (object location &rest more-locations)
  (let ((frame (read-frame object))
        (location (location-list (cons location more-locations)
                                 (read-dimension object)
                                 (access-position object))))
    (ensure-dimensionality frame location)
    (ensure-in-frame frame location)
    (nth-value 0 (at-data (read-data object) location))))


(defun (setf at-cell) (new-value object location &rest more-locations)
  (let ((frame (read-frame object))
        (location (location-list (cons location more-locations)
                                 (read-dimension object)
                                 (access-position object))))
    (ensure-dimensionality frame location)
    (ensure-in-frame frame location)
    (set-at-data new-value (read-data object) location)
    (cl-ds:force new-value)))
