(cl:in-package #:cl-data-structures.file-system)


(defclass tokenizing-range (cl-ds:chunking-mixin
                            file-range-mixin
                            cl-ds:fundamental-forward-range)
  ((%path :initarg :path
          :type (or string pathname)
          :reader read-path)
   (%regex :initarg :regex
           :reader read-regex))
  (:default-initargs :initial-position 0))


(defmethod cl-ds:clone ((range tokenizing-range))
  (make 'tokenizing-range
        :path (read-path range)
        :regex (read-regex range)
        :reached-end (access-reached-end range)
        :initial-position (access-current-position range)))


(defmethod cl-ds:peek-front ((range tokenizing-range))
  (if (access-reached-end range)
      (values nil nil)
      (iterate
        (with stream = (ensure-stream range))
        (with buffer = (make-array 0
                                   :element-type 'character
                                   :adjustable t
                                   :fill-pointer 0))
        (with regex = (read-regex range))
        (with file-position = (file-position stream))
        (for character = (read-char stream :eof-value nil))
        (when (null character)
          (leave (values nil nil)))
        (vector-push-extend character buffer)
        (when (cl-ppcre:all-matches regex buffer)
          (unless (file-position (read-stream range)
                                 file-position)
            (error 'cl-ds:file-releated-error
                   :path (read-path range)
                   :format-control "Can't change position in the stream."))
          (leave (values buffer t))))))


(defmethod cl-ds:consume-front ((range tokenizing-range))
  (if (access-reached-end range)
      (values nil nil)
      (iterate
        (with stream = (ensure-stream range))
        (with buffer = (make-array 0
                                   :element-type 'character
                                   :adjustable t
                                   :fill-pointer 0))
        (with regex = (read-regex range))
        (for character = (read-char stream :eof-value nil))
        (setf (access-current-position range) (file-position stream))
        (when (null character)
          (setf (access-reached-end range) t)
          (close-stream range)
          (leave (values nil nil)))
        (vector-push-extend character buffer)
        (when (cl-ppcre:all-matches regex buffer)
          (leave (values buffer t))))))


(defmethod cl-ds:traverse ((range tokenizing-range) function)
  (cl-ds.fs:with-file-ranges ((r range))
    (declare (ignore r))
    (call-next-method)))


(defun tokenize (path regex &key case-insensitive-mode)
  (check-type path stream-designator)
  (let ((scanner (cl-ppcre:create-scanner
                  regex :case-insensitive-mode case-insensitive-mode)))
    (make 'tokenizing-range
          :path path
          :regex scanner)))
