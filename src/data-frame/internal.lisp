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


(defun apply-aliases (aliases locations)
  (iterate
    (for loc on locations)
    (for x = (car loc))
    (for i from 0)
    (when (symbolp x)
      (let ((result (gethash (cons i x) aliases)))
        (when (null result)
          (error 'cl-ds:invalid-argument
                 :text "Unkown alias."
                 :argument x))
        (setf (car loc) result)))
    (finally (return locations))))
