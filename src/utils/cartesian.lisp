(in-package #:cl-data-structures.utils)


(defun cartesian (vector-of-vectors result-callback)
  (let* ((l (length vector-of-vectors))
         (lengths (map '(vector fixnum) #'length vector-of-vectors))
         (indexes (make-array l :element-type 'fixnum)))
    (iterate
      (for i = (iterate
                 (for i from 0 below l)
                 (finding i such-that (< (1+ (aref indexes i))
                                         (aref lengths i)))))
      (for p-i previous i initially 0)
      (apply result-callback
             (map 'list #'aref vector-of-vectors indexes))
      (until (null i))
      (when (not (eql i p-i))
        (iterate (for j from 0 below i)
          (setf (aref indexes j) 0)))
      (incf (aref indexes i)))))
