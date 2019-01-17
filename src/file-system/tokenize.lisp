(in-package #:cl-data-structures.file-system)


(defclass tokenizing-range (cl-ds:chunking-mixin
                            file-range-mixin
                            cl-ds:fundamental-forward-range)
  ((%path :initarg :path
          :type (or string pathname)
          :reader read-path)
   (%reached-end :initarg :reached-end
                 :type boolean
                 :accessor access-reached-end
                 :initform nil)
   (%regex :initarg :regex
           :reader read-regex)
   (%current-position :initarg :initial-position
                      :accessor access-current-position
                      :type non-negative-integer)
   (%initial-position :initarg :initial-position
                      :type non-negative-integer
                      :reader read-initial-position))
  (:default-initargs :initial-position 0))


(defmethod cl-ds:clone ((range tokenizing-range))
  (make 'tokenizing-range
        :path (read-path range)
        :regex (read-regex range)
        :reached-end (access-reached-end range)
        :initial-position (access-current-position range)))


(defmethod cl-ds:reset! ((range tokenizing-range))
  (setf (access-current-position range) (read-initial-position range))
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
        (let* ((stream (read-stream range))
               (file-position (file-position stream))
               (buffer (make-array 0
                                   :element-type 'character
                                   :adjustable t
                                   :fill-pointer 0))
               (regex (read-regex range)))
          (iterate
            (for character = (read-char stream :eof-value nil))
            (when (null character)
              (leave (values nil nil)))
            (vector-push-extend character buffer)
            (when (cl-ppcre:all-matches regex buffer)
              (unless (file-position (read-stream range)
                                     file-position)
                (error 'cl-ds:textual-error
                       :text "Can't change position in the stream."))
              (leave (values buffer t))))))))


(defmethod cl-ds:consume-front ((range tokenizing-range))
  (if (access-reached-end range)
      (values nil nil)
      (progn
        (ensure-stream range)
        (let* ((stream (read-stream range))
               (buffer (make-array 0
                                   :element-type 'character
                                   :adjustable t
                                   :fill-pointer 0))
               (regex (read-regex range)))
          (iterate
            (for character = (read-char stream :eof-value nil))
            (setf (access-current-position range) (file-position stream))
            (when (null character)
              (leave (values nil nil)))
            (vector-push-extend character buffer)
            (when (cl-ppcre:all-matches regex buffer)
              (leave (values buffer t))))))))


(defun tokenize (path regex &key case-insensitive-mode)
  (let ((scanner (cl-ppcre:create-scanner
                  regex :case-insensitive-mode case-insensitive-mode)))
    (make 'tokenizing-range
          :path path
          :regex scanner)))
