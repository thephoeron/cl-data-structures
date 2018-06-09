(in-package #:cl-data-structures.data-frame)


(defun column-at (number)
  (rcurry #'column number))


(defun row-at (number)
  (rcurry #'row number))


(defun validate-data (more-data)
  (let* ((data (first-elt more-data))
         (data-dimensionality (cl-ds:dimensionality data))
         (data-size (cl-ds:size data)))
    (unless (every (compose (curry #'= data-dimensionality)
                            #'cl-ds:dimensionality)
                   more-data)
      (error 'cl-ds:initialization-error
             :class 'data-frame
             :text "Can't create data-frame because data provided is of unequal dimensionality."))
    (unless (every (compose (curry #'= data-size)
                            #'cl-ds:size)
                   more-data)
      (error 'cl-ds:initialization-error
             :class 'data-frame
             :text "Can't create data-frame because data provided is of unequal sizes."))))


(defun initialize-dimensions (data-frame dimension)
  (iterate
    (repeat dimension)
    (for data initially (access-data data-frame)
         then (lret ((next (cl-ds.seqs.rrb:make-mutable-rrb-vector)))
                (cl-ds:put! data next)))))


(defun fill-dimensions (data-frame data dimension)
  (let ((size (array-dimension data 0)))
    (iterate
      (for d = (iterate
                 (for d in-vector data)
                 (for (values v more) = (cl-ds:consume-front d))
                 (unless more
                   (return-from fill-dimensions nil))
                 (collect v))))))



(defun stack (dimension key data &rest more-data)
  (let* ((data-dimensionality (cl-ds:dimensionality data))
         (data-size (cl-ds:size data))
         (frame-dimensionality (1+ data-dimensionality))
         (result-content (cl-ds.seqs.rrb:make-mutable-rrb-vector))
         (data (coerce (cons data more-data) 'vector))
         (result-frame (make-instance 'data-frame
                                      :data result-content
                                      :dimensionality frame-dimensionality)))
    (validate-data data)
    (initialize-dimensions result-frame dimension)
    (fill-dimensions result-frame data dimension)))


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
