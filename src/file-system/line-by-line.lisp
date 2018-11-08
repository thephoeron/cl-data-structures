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
   (%initial-position :initarg :initial-position
                      :initform 0
                      :type non-negative-integer
                      :reader read-initial-position)))


(defmethod cl-ds:clone ((range line-by-line-range))
  (make 'line-by-line-range
        :path (read-path range)
        :stream nil
        :reached-end (access-reached-end range)
        :initial-position (if (read-stream range)
                              (file-position (read-stream range))
                              0)))


(defun ensure-stream (range)
  (when (~> range read-stream null)
    (setf (car (slot-value range '%stream)) (~> range read-path open))))


(defmethod cl-ds:reset! ((range line-by-line-range))
  (ensure-stream range)
  (unless (file-position (read-stream range)
                         (read-initial-position range))
    (error 'cl-ds:textual-error
           :text "Can't change position in the stream."))
  (setf (access-reached-end range) nil)
  range)


(defmethod cl-ds:peek-front ((range line-by-line-range))
  (if (access-reached-end range)
      (values nil nil)
      (progn
        (ensure-stream range)
        (let ((file-position (~> range read-stream file-position))
              (line (~> range read-stream
                        (read-line :eof-value nil))))
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
        (let ((line (~> range read-stream
                        (read-line nil nil))))
          (if (null line)
              (progn
                (close-stream range)
                (setf (access-reached-end range) t)
                (values nil nil))
              (values line t))))))


(defmethod cl-ds:traverse (function (range line-by-line-range))
  (unless (~> range access-reached-end)
    (unwind-protect
         (ensure-stream range)
         (iterate
           (with stream = (read-stream range))
           (for line = (read-line stream nil nil))
           (until (null line))
           (funcall function line))
      (setf (access-reached-end range) t)
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
        :path path
        :stream (open path)))
