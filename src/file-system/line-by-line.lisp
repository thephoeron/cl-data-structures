(in-package #:cl-data-structures.file-system)


(defclass line-by-line-range (cl-ds:chunking-mixin
                              cl-ds:fundamental-forward-range
                              file-range-mixin)
  ((%path :initarg :path
          :type (or string pathname)
          :reader read-path)
   (%reached-end :initarg :reached-end
                 :type boolean
                 :accessor access-reached-end
                 :initform nil)
   (%current-position :initarg :initial-position
                      :accessor access-current-position
                      :type non-negative-integer)
   (%initial-position :initarg :initial-position
                      :type non-negative-integer
                      :reader read-initial-position))
  (:default-initargs :initial-position 0))


(defmethod cl-ds:clone ((range line-by-line-range))
  (make 'line-by-line-range
        :path (read-path range)
        :reached-end (access-reached-end range)
        :initial-position (read-current-position range)))


(defun ensure-stream (range)
  (when (~> range read-stream null)
    (let ((file (~> range read-path open)))
      (unless (file-position file (read-initial-position range))
        (error 'cl-ds:textual-error
               :text "Can't change position in the stream."))
      (setf (car (slot-value range '%stream)) file
            (access-current-position range) (read-initial-position range)))))


(defmethod cl-ds:reset! ((range line-by-line-range))
  (ensure-stream range)
  (unless (file-position (read-stream range)
                         (read-initial-position range))
    (error 'cl-ds:textual-error
           :text "Can't change position in the stream."))
  (setf (access-reached-end range) nil
        (access-current-position range) (read-initial-position range))
  range)


(defmethod cl-ds:peek-front ((range line-by-line-range))
  (if (access-reached-end range)
      (values nil nil)
      (progn
        (ensure-stream range)
        (let* ((stream (read-stream range))
               (file-position (file-position stream))
               (line (read-line stream :eof-value nil)))
          (if (null line)
              (values nil nil)
              (progn
                (unless (file-position (read-stream range)
                                       file-position)
                  (error 'cl-ds:textual-error
                         :text "Can't change position in the stream."))
                (values line t)))))))


(defmethod cl-ds:consume-front ((range line-by-line-range))
  (if (~> range access-reached-end)
      (values nil nil)
      (progn
        (ensure-stream range)
        (let* ((stream (read-stream range))
               (line (read-line stream nil nil)))
          (setf (access-current-position range) (file-position stream))
          (if (null line)
              (progn
                (setf (access-reached-end range) t)
                (close-stream range)
                (values nil nil))
              (values line t))))))


(defmethod cl-ds:traverse (function (range line-by-line-range))
  (unless (~> range access-reached-end)
    (ensure-stream range)
    (unwind-protect
         (iterate
           (with stream = (read-stream range))
           (for line = (read-line stream nil nil))
           (until (null line))
           (funcall function line))
      (setf (access-reached-end range) t
            (access-current-position range) (~> range read-stream file-position))
      (close-stream range)))
  range)


(defmethod cl-ds:across (function (range line-by-line-range))
  (unless (~> range access-reached-end)
    (let ((initial-position (if (read-stream range)
                                (~> range
                                    read-stream
                                    file-position)
                                0)))
      (with-open-file (stream (read-path range))
        (unless (file-position stream initial-position)
          (error 'cl-ds:textual-error
                 :text "Can't change position in the stream."))
        (iterate
          (for line = (read-line stream nil nil))
          (until (null line))
          (funcall function line)))))
  range)


(defun line-by-line (path)
  (check-type path (or string pathname))
  (make 'line-by-line-range
        :path path))
