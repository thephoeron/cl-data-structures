(in-package #:cl-data-structures.data-frame)


(defun at-data (data location)
  (if (endp location)
      data
      (at-data (cl-ds:at data
                         (first location))
               (rest location))))


(defun set-at-data (new-value data location)
  (if (endp (rest location))
      (setf (cl-ds:at data (first location))
            new-value)
      (set-at-data new-value
                   (cl-ds:at data
                             (first location))
                   (rest location))))


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
    (finally (return address))))


(defun apply-aliases (aliases locations)
  (iterate
    (for loc on locations)
    (for x = (car loc))
    (for i from 0)
    (setf (car loc) (if (symbolp x)
                        (lret ((result (gethash (cons i x) aliases)))
                          (when (null result)
                            (error 'cl-ds:invalid-argument
                                   :text "Unkown alias."
                                   :argument x)))
                        x))
    (finally (return locations))))
