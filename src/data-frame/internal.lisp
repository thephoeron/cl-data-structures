(in-package #:cl-data-structures.data-frame)


(defun at-data (data location)
  (if (endp location)
      data
      (at-data (cl-ds:at data
                         (first location))
               (rest location))))


(defun set-at-data (new-value data location)
  (let ((first (first location))
        (rest (rest location)))
    (if (endp rest)
        (setf (cl-ds:at data first)
              new-value)
        (let ((value (cl-ds:at data first)))
          (setf (cl-ds:at data first) value)
          (set-at-data new-value
                       value
                       rest)))))


(defun location-list (address dimension location)
  (iterate
    (for loc on address)
    (for prev-loc on address)
    (for i from 0)
    (when (eql i dimension)
      (if (zerop i)
          (leave (cons location address))
          (progn
            (setf (cdr prev-loc) (cons location loc))
            (leave address))))
    (finally (progn (setf (rest loc) (list location))
                    (return address)))))


(defun apply-alias (aliases dimension position)
  (when (null position)
    (error 'type-error :expected-type '(or symbol fixnum)
           :datum position))
  (if (symbolp position)
      (lret ((result (gethash (cons dimension position) aliases)))
        (when (null result)
          (error 'cl-ds:invalid-argument
                 :text "Unkown alias."
                 :argument position)))
      position))


(defun apply-aliases (aliases locations)
  (iterate
    (for loc on locations)
    (for x = (car loc))
    (for i from 0)
    (setf (car loc) (apply-alias aliases i x))
    (finally (return locations))))


(defun select-bounds (bounds locations)
  (iterate
    (with j = 0)
    (with result = (~> bounds
                       length
                       (- (length locations))
                       (make-array :element-type 'non-negative-fixnum)))
    (for i from 0)
    (for val in-vector bounds)
    (when (eql i (caar locations))
      (setf locations (rest locations))
      (next-iteration))
    (setf (aref result j) val)
    (incf j)
    (finally (return result))))


(defun proxy-plane (data locations)
  (iterate
    (for (axis position) in locations)
    (unless (< axis (cl-ds:dimensionality data))
      (error 'cl-ds:dimensionality-error
             :value axis
             :bounds `(0 ,(cl-ds:dimensionality data))
             :text "No such axis in the frame."))
    (unless (<= #1=(aref (read-lower-bounds data) axis)
                position
                #2=(1- (aref (read-upper-bounds data) axis)))
      (error 'cl-ds:argument-out-of-bounds
             :value position
             :bounds (list #1# #2#)
             :text "No such position on axis.")))
  (let ((old-aliases (read-reverse-aliases data))
        (old-dimensionality (cl-ds:dimensionality data))
        (revert-aliases (make-hash-table :test 'equal))
        (new-aliases (make-hash-table :test 'equal)))
    (iterate
      (for (key position) in-hashtable old-aliases)
      (for (dimension . name) = key)
      (for found = (find dimension locations :key #'first))
      (when found
        (next-iteration))
      (for index = (or (position-if (lambda (x) (> x dimension))
                                    locations
                                    :key #'car)
                       0))
      (for new-dimension = (- dimension index))
      (setf (gethash (cons new-dimension position) new-aliases) name
            (gethash (cons new-dimension name) revert-aliases) position))
    (make 'proxy-data-frame
          :lower-bounds (select-bounds (read-lower-bounds data)
                                       locations)
          :upper-bounds (select-bounds (read-upper-bounds data)
                                       locations)
          :reverse-alias revert-aliases
          :dimensionality (- old-dimensionality (length locations))
          :aliases new-aliases
          :inner-data-frame data
          :pinned-axis locations)))


(defun at-data-frame (data-frame address)
  (ensure-dimensionality data-frame address)
  (apply-aliases (read-aliases data-frame) address)
  (ensure-in-frame data-frame address)
  (at-data (access-data data-frame)
           address))


(defun set-at-data-frame (new-value data-frame address)
  (ensure-dimensionality data-frame address)
  (apply-aliases (read-aliases data-frame) address)
  (ensure-in-frame data-frame address)
  (set-at-data new-value
               (access-data data-frame)
               address))


(defun insert-pinned-axis (object address)
  (lret ((result (~> object
                     read-inner-data-frame
                     cl-ds:dimensionality
                     make-list)))
    (iterate
      (with axis = (read-pinned-axis object))
      (for dimension = (caar axis))
      (for cell on result)
      (for i from 0)
      (if (eql i dimension)
          (setf (car cell) (cadar axis)
                axis (rest axis))
          (setf (car cell) (first address)
                address (rest address))))))


(defun common-mutate! (dimension accessor data function ranges)
  (let ((*active-data* accessor))
    (block outer
      (iterate
        (for i
             from (~> data read-lower-bounds (aref dimension))
             below (~> data read-upper-bounds (aref dimension)))
        (for extra-data =
             (iterate
               (for range in ranges)
               (for (values value more) = (cl-ds:consume-front range))
               (unless more
                 (return-from outer))
               (collect value)))
        (setf (access-position *active-data*) i)
        (apply function extra-data)))))
