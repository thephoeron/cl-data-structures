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


(defun initialize-dimensions (sizes)
  (assert sizes)
  (lret ((result (cl-ds:make-of-size 'cl-ds.seqs.rrb:make-mutable-rrb-vector
                                     (first sizes))))
    (when (rest sizes)
      (iterate
        (for i from 0 below (first sizes))
        (setf (cl-ds:at result i) (initialize-dimensions (rest sizes)))))))


(defun initialize-sizes (dimension data)
  (validate-data data)
  (coerce
   (iterate
     (for d in-vector data)
     (for i from 0)
     (collect (if (eql i dimension)
                  (length data)
                  (cl-ds:size d))))
   '(vector non-negative-fixnum)))


(defun fill-data (data-frame axis position data)
  (bind ((addres (map 'list
                      (constantly 0)
                      (read-sizes data-frame)))
         (length (~> data-frame read-sizes length)))
    (setf (elt addres axis) position)
    (iterate
      (for i from 0 below (aref (read-sizes data-frame) axis))
      (iterate
        (for j from 0 below (cl-ds:size data))
        (iterate
          (for k from 0 below length)
          (for adr on addres)
          (if (eql k axis)
              (setf (car adr) i)
              (setf (car adr) j))) ; TODO support for multiple dimensions
        (apply #'(setf cl-ds:at) (cl-ds:at data i) data-frame addres)))))


(defun fill-dimensions (data-frame data dimension)
  (iterate
    (for j from 0)
    (for d in-vector data)
    (fill-data data-frame dimension j d)))


(defun stack (dimension data &rest more-data)
  (let* ((data-dimensionality (cl-ds:dimensionality data))
         (frame-dimensionality (1+ data-dimensionality))
         (data (coerce (cons data more-data) 'vector))
         (sizes (initialize-sizes dimension data))
         (result-content (initialize-dimensions sizes))
         (result-frame (make 'data-frame
                             :data result-content
                             :lower-bounds (make-array
                                            frame-dimensionality
                                            :element-type 'non-negative-fixnum
                                            :initial-element 0)
                             :upper-bounds sizes
                             :dimensionality frame-dimensionality)))
    (fill-dimensions result-frame
                     data
                     dimension)
    result-frame))


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
