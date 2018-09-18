(in-package #:cl-data-structures.file-system)


(defclass find-range (cl-ds:chunking-mixin
                      cl-ds:fundamental-forward-range)
  ((%stack :initarg :stack
           :initform nil
           :type list
           :accessor access-stack)))


(defgeneric find (description)
  ())


;; ((:directory :path "/home/shka/Muzyka")
;;  (:regex-directory :path "*"
;;   :times '(0 :recursive))
;;  (:regex-file :path "*.lisp"))


(defclass fundamental-file-range-stack-cell ()
  ((%prev-cell :initarg :prev-cell
               :initform nil
               :type fundamental-file-range-stack-cell
               :accessor access-prev-cell)
   (%path :initarg :path
          :type string
          :reader read-path)))


(defclass regex-file-range-stack-cell (fundamental-file-range-stack-cell)
  ((%state :initform nil
           :accessor access-state
           :type list
           :initarg :state)))


(defclass directory-file-range-stack-cell (fundamental-file-range-stack-cell)
  ())


(defclass regex-directory-file-range-stack-cell (directory-file-range-stack-cell
                                                 regex-file-range-stack-cell)
  ((%times :initarg :times
           :reader read-times
           :initform 1)))


(defclass file-file-range-stack-cell (fundamental-file-range-stack-cell)
  ())


(defclass regex-file-file-range-stack-cell (fundamental-file-range-stack-cell
                                            regex-file-range-stack-cell)
  ())


(defgeneric make-stack-cell (name &key &allow-other-keys))


(defmethod make-stack-cell ((name (eql :directory)) &key path)
  (make 'directory-file-range-stack-cell
        :path path))


(defmethod make-stack-cell ((name (eql :regex-directory)) &key path times)
  (make 'regex-directory-file-range-stack-cell
        :path path
        :times times))


(defmethod make-stack-cell ((name (eql :regex-file)) &key path)
  (make 'regex-directory-file-range-stack-cell
        :path path))
