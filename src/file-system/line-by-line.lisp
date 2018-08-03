(in-package #:cl-data-structures.file-system)


(defclass line-by-line-range (cl-ds:fundamental-forward-range)
  ((%path :initarg :path
          :type (or string pathname)
          :reader read-path)
   (%input-stream :initarg :input-stream
                  :type stream
                  :reader read-input-stream)
   (%initial-position :initarg :initial-position
                      :initform 0
                      :type non-negative-integer
                      :reader read-initial-position)))


(defmethod initialize-instance :after ((range line-by-line-range)
                                       &rest all)
  (declare (ignore all))
  (trivial-garbage:finalize (read-input-stream range) #'close))


(defmethod cl-ds:clone ((range line-by-line-range))
  (let ((stream (~> range read-path open))
        (position (~> range read-input-stream file-position)))
    (unless position
      (error 'cl-ds:textual-error
             :text "Can't determine position in the stream."))
    (unless (file-position stream position)
      (error 'cl-ds:textual-error
             :text "Can't change position in the stream."))
    (make 'line-by-line-range
          :path (read-path range)
          :input-stream :stream
          :initial-position position)))


(defmethod cl-ds:reset! ((range line-by-line-range))
  (unless (file-position (read-input-stream range)
                         (read-initial-position range))
    (error 'cl-ds:textual-error
           :text "Can't change position in the stream."))
  range)


(defmethod cl-ds:peek-front ((range line-by-line-range))
  (let ((file-position (~> range read-input-stream file-position))
        (line (~> range read-input-stream
                  (read-line :eof-value nil))))
    (if (null line)
        (values nil nil)
        (progn
          (unless (file-position (read-input-stream range)
                                 file-position)
            (error 'cl-ds:textual-error
                   :text "Can't change position in the stream."))
          (values line t)))))


(defmethod cl-ds:consume-front ((range line-by-line-range))
  (let ((line (~> range read-input-stream
                  (read-line :eof-value nil))))
    (if (null line)
        (values nil nil)
        (values line t))))


(defmethod cl-ds:traverse (function (range line-by-line-range))
  (let ((initial-position (~> range
                              read-initial-position
                              file-position)))
    (with-open-file (stream (read-path range))
      (unless (file-position stream initial-position)
        (error 'cl-ds:textual-error
               :text "Can't change position in the stream."))
      (iterate
        (for line = (read-line stream :eof-value nil))
        (until (null line))
        (funcall function line)
        (finally (return range))))))


(defun line-by-line (path)
  (check-type path (or string pathname))
  (make 'line-by-line-range
        :path path
        :input-stream (open path)))
