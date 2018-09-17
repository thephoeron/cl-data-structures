(in-package #:cl-data-structures.file-system)


(defclass line-by-line-range (cl-ds:chunking-mixin
                              cl-ds:fundamental-forward-range
                              file-range-mixin)
  ((%path :initarg :path
          :type (or string pathname)
          :reader read-path)
   (%initial-position :initarg :initial-position
                      :initform 0
                      :type non-negative-integer
                      :reader read-initial-position)))


(defmethod cl-ds:clone ((range line-by-line-range))
  (let ((stream (~> range read-path open))
        (position (~> range read-stream file-position)))
    (unless position
      (error 'cl-ds:textual-error
             :text "Can't determine position in the stream."))
    (unless (file-position stream position)
      (error 'cl-ds:textual-error
             :text "Can't change position in the stream."))
    (make 'line-by-line-range
          :path (read-path range)
          :stream stream
          :initial-position position)))


(defun ensure-stream (range)
  (when (~> range read-stream null)
    (setf (slot-value range '%stream) (~> range read-path open))))


(defmethod cl-ds:reset! ((range line-by-line-range))
  (ensure-stream range)
  (unless (file-position (read-stream range)
                         (read-initial-position range))
    (error 'cl-ds:textual-error
           :text "Can't change position in the stream."))
  range)


(defmethod cl-ds:peek-front ((range line-by-line-range))
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
          (values line t)))))


(defmethod cl-ds:consume-front ((range line-by-line-range))
  (if (~> range read-stream null)
      (values nil nil)
      (let ((line (~> range read-stream
                      (read-line nil nil))))
        (if (null line)
            (progn
              (close-stream range)
              (values nil nil))
            (values line t)))))


(defmethod cl-ds:traverse (function (range line-by-line-range))
  (unless (~> range read-stream null)
    (unwind-protect
         (iterate
           (with stream = (read-stream range))
           (for line = (read-line stream nil nil))
           (until (null line))
           (funcall function line)
           (finally (return range)))
      (close-stream range))))


(defmethod cl-ds:across (function (range line-by-line-range))
  (unless (~> range read-stream null)
    (let ((initial-position (~> range
                                read-stream
                                file-position)))
      (with-open-file (stream (read-path range))
        (unless (file-position stream initial-position)
          (error 'cl-ds:textual-error
                 :text "Can't change position in the stream."))
        (iterate
          (for line = (read-line stream nil nil))
          (until (null line))
          (funcall function line)
          (finally (return range)))))))


(defun line-by-line (path)
  (check-type path (or string pathname))
  (make 'line-by-line-range
        :path path
        :stream (open path)))
