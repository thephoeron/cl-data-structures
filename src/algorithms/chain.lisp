(in-package #:cl-data-structures.algorithms)


(defclass forward-chain-of-ranges (cl-ds:fundamental-forward-range)
  ((%content :initarg :content
             :reader read-content)
   (%original-content :initarg :original-content
                      :type list
                      :reader read-original-content)))


(defclass bidirectional-chain-of-ranges (forward-chain-of-ranges
                                         fundamental-bidirectional-range)
  ())


(defclass random-access-chain-of-ranges (bidirectional-chain-of-ranges
                                         fundamental-random-access-range)
  ())


(defun init-chain-of-range (obj)
  (bind (((:slots %content %original-content) obj))
    (setf %content (make 'flexichain:standard-flexichain))
    (map nil
         (lambda (x) (~>> x cl-ds:clone (flexichain:push-end %content)))
         %original-content)))


(defmethod initialize-instance :after ((obj forward-chain-of-ranges) &key &allow-other-keys)
  (init-chain-of-range obj))


(defmethod reinitialize-instance ((obj forward-chain-of-ranges) &key &allow-other-keys)
  (init-chain-of-range obj))


(defun chain (&rest ranges)
  (map nil
       (lambda (x) (check-type x cl-ds:fundamental-forward-range))
       ranges)
  (let ((fundamental-type (common-fundamental-range-class ranges)))
    (assert fundamental-type)
    (make (eswitch (fundamental-type)
            ('fundamental-forward-range 'forward-chain-of-ranges)
            ('fundamental-bidirectional-range 'bidirectional-chain-of-ranges)
            ('fundamental-random-access-range 'random-access-chain-of-ranges))
          :original-content ranges)))


(defmethod cl-ds:reset! ((range forward-chain-of-ranges))
  (reinitialize-instance range)
  range)


(defmethod cl-ds:at ((range random-access-chain-of-ranges) location)
  (cl-ds.utils:todo))


(defmethod cl-ds:size ((range random-access-chain-of-ranges))
  (bind (((:slots %content) range)
         (count (flexichain:nb-elements %content)))
    (iterate
      (for i below count)
      (sum (~> %content (flexichain:element* i) cl-ds:size)))))


(defmethod cl-ds:consume-front ((range forward-chain-of-ranges))
  (bind (((:slots %content) range))
    (iterate
      (if (zerop (flexichain:nb-elements %content))
          (leave (values nil nil))
          (let ((front (flexichain:element* %content 0)))
            (if (cl-ds:morep front)
                (leave (cl-ds:consume-front front))
                (flexichain:pop-start %content)))))))


(defmethod cl-ds:peek-front ((range forward-chain-of-ranges))
  (bind (((:slots %content) range))
    (iterate
      (if (zerop (flexichain:nb-elements %content))
          (leave (values nil nil))
          (let ((front (flexichain:element* %content 0)))
            (if (cl-ds:morep front)
                (leave (cl-ds:peek-front front))
                (flexichain:pop-start %content)))))))


(defmethod cl-ds:consume-back ((range bidirectional-chain-of-ranges))
  (bind (((:slots %content) range))
    (iterate
      (for count = (flexichain:nb-elements %content))
      (if (zerop count)
          (leave (values nil nil))
          (let ((back (flexichain:element* %content (1- count))))
            (if (cl-ds:morep back)
                (leave (cl-ds:consume-back back))
                (flexichain:pop-end %content)))))))


(defmethod cl-ds:peek-back ((range bidirectional-chain-of-ranges))
  (bind (((:slots %content) range))
    (iterate
      (for count = (flexichain:nb-elements %content))
      (if (zerop count)
          (leave (values nil nil))
          (let ((back (flexichain:element* %content (1- count))))
            (if (cl-ds:morep back)
                (leave (cl-ds:peek-back back))
                (flexichain:pop-end %content)))))))


(defmethod cl-ds:empty-clone ((range forward-chain-of-ranges))
  (make (type-of range)
        :original-content (read-original-content range)))


(defmethod cl-ds:traverse (function (range forward-chain-of-ranges))
  (bind (((:slots %content) range))
    (iterate
      (for i from 0 below (flexichain:nb-elements %content))
      (cl-ds:traverse function (flexichain:element* %content i)))
    range))


(defmethod cl-ds:morep ((range forward-chain-of-ranges))
  (bind (((:slots %content) range)
         (count (flexichain:nb-elements %content)))
    (iterate
      (for i below count)
      (finding t such-that (~> %content
                               (flexichain:element* i)
                               cl-ds:morep)))))
