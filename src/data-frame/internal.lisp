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


(defun proxy-plane (data locations)
  (let ((old-aliases (read-reverse-aliases data))
        (old-dimensionality = (cl-ds:dimensionality data))
        (new-aliases (make-hash-table :test 'eql))
        (new-reverse-alias (make-hash-table :test 'equal)))
    (iterate
      (for (key value) in-hashtable old-aliases)
      (for (dimension . position) = key)
      (for found = (find dimension locations :key #'first))
      (when found
        (next-iteration))
      (for index = (or (position-if (lambda (x) (> x dimension))
                                    locations
                                    :key #'car)
                       1))
      (for new-dimension = (- dimension (1- index)))
      (setf (gethash (cons new-dimension position) new-aliases) value))
    (make 'proxy-data-frame
          :inner-data-frame data
          :aliases (~> data read-aliases copy-hash-table)
          :dimensionality (- (cl-ds:dimensionality data)
                             (length locations))
          :pinned-axis locations)))


(defun at-data-frame (data-frame address)
  (ensure-dimensionality data-frame address)
  (ensure-in-frame data-frame address)
  (at-data (access-data data-frame)
           address))


(defun set-at-data-frame (new-value data-frame address)
  (ensure-dimensionality data-frame address)
  (ensure-in-frame data-frame address)
  (set-at-data new-value
               (access-data data-frame)
               address))


(defun proxy-data-frame-effective-address (data-frame more)
  (iterate
    (with axis = (read-pinned-axis data-frame))
    (with effective-address = (make-list (cl-ds:dimensionality data-frame)))
    (for addr on effective-address)
    (for i from 0)
    (if (eql (caar axis) i)
        (setf (car addr) (cadar axis)
              axis (rest axis))
        (setf (car addr) (first more)
              more (rest more)))
    (finally (return (apply-aliases data-frame effective-address)))))
