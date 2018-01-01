(in-package #:cl-data-structures.algorithms)


(defclass proxy-range ()
  ((%original-range :initarg :original-range
                    :reader read-original-range)))


(defmethod cl-ds:empty-clone-of-inner-container ((range proxy-range))
  (cl-ds:empty-clone-of-inner-container (read-original-range range)))


(defmethod cl-ds:traverse (function (range proxy-range))
  (cl-ds:traverse function (read-original-range range)))


(defclass forward-proxy-range (proxy-range fundamental-forward-range)
  ())


(defclass bidirectional-proxy-range (proxy-range fundamental-bidirectional-range)
  ())


(defclass random-access-proxy-range (proxy-range fundamental-random-access-range)
  ())


(defgeneric make-proxy (range class
                        &rest all
                        &key &allow-other-keys)
  (:method ((range fundamental-range)
            class &rest all &key &allow-other-keys)
    (apply #'make-instance class :original-range range all)))


(defclass hash-table-range (fundamental-random-access-range
                            key-value-range)
  ((%hash-table :initarg :hash-table
                :reader read-hash-table)
   (%keys :initarg :keys
          :reader read-keys)
   (%begin :initarg :begin
           :type fixnum
           :accessor access-begin)
   (%end :initarg :end
         :type fixnum
         :accessor access-end)))


(defmethod print-object ((obj hash-table-range) stream)
  (format stream "<HT-range:~%")
  (let ((count 10))
    (block map-block
      (maphash (lambda (key value)
                 (format stream " ~a : ~a~%" key value)
                 (decf count)
                 (when (zerop count)
                   (return-from map-block)))
               (read-hash-table obj))
      (when (> (hash-table-count (read-hash-table obj)) 10)
        (format stream " ~a~%" "..."))
      (format stream ">")
      obj)))


(defmethod cl-ds:traverse (function (range hash-table-range))
  (let ((keys (read-keys range))
        (table (read-hash-table range)))
    (iterate
      (for i from (access-begin range) below (access-end range))
      (for key = (aref keys i))
      (funcall function (list* key (gethash key table))))))


(defmethod cl-ds:empty-clone-of-inner-container ((range hash-table-range))
  (let ((original-table (read-hash-table range)))
    (make-hash-table
     :test (hash-table-test original-table)
     :hash-function (hash-table-function original-table)
     :rehash-size (hash-table-rehash-size original-table)
     :rehash-threshold (hash-table-rehash-threshold original-table))))


(defmethod clone ((range hash-table-range))
  (make 'hash-table-range
        :keys (read-keys range)
        :begin (access-begin range)
        :end (access-end range)
        :hash-table (read-hash-table range)))


(defmethod consume-front ((range forward-proxy-range))
  (consume-front (read-original-range range)))


(defmethod consume-back ((range bidirectional-proxy-range))
  (consume-back (read-original-range range)))


(defmethod peek-front ((range forward-proxy-range))
  (peek-front (read-original-range range)))


(defmethod peek-back ((range bidirectional-proxy-range))
  (peek-back (read-original-range)))


(defmethod at ((range random-access-proxy-range) location)
  (at (read-original-range range) location))


(defmethod morep ((range forward-proxy-range))
  (morep (read-original-range range)))


(defmethod consume-front ((range hash-table-range))
  (bind (((:slots (begin %begin) (end %end) (ht %hash-table) (keys %keys)) range)
         ((:lazy key result)
          (prog1 (aref keys begin) (incf begin))
          (list* key (gethash key ht))))
    (if (eql begin end)
        (values nil nil)
        (values result t))))


(defmethod consume-back ((range hash-table-range))
  (bind (((:slots (begin %begin) (end %end) (ht %hash-table) (keys %keys)) range)
         ((:lazy key result)
          (aref keys (decf end))
          (list* key (gethash key ht))))
    (if (eql begin end)
        (values nil nil)
        (values result t))))


(defmethod peek-front ((range hash-table-range))
  (bind (((:slots (begin %begin) (end %end) (ht %hash-table) (keys %keys)) range)
         ((:lazy key result) (aref keys begin) (list* key (gethash key ht))))
    (if (eql begin end)
        (values nil nil)
        (values result t))))


(defmethod peek-back ((range hash-table-range))
  (bind (((:slots (begin %begin) (end %end) (ht %hash-table) (keys %keys)) range)
         ((:lazy key result) (aref keys end) (list* key (gethash key ht))))
    (if (eql begin end)
        (values nil nil)
        (values result t))))


(defmethod drop-front ((range hash-table-range) count)
  (bind (((:slots (begin %begin) (end %end)) range)
         (count (min end (+ begin count))))
    (setf begin count))
  range)


(defmethod drop-back ((range hash-table-range) count)
  (with-slots ((begin %begin) (end %end)) range
    (setf end (max begin (- end count))))
  range)


(defmethod morep ((range hash-table-range))
  (with-slots ((begin %begin) (end %end)) range
    (not (eql begin end))))


(defmethod at ((range hash-table-range) location)
  (bind (((:slots (ht %hash-table)) range)
         ((:hash-table (result location)) ht)
         (test (hash-table-test ht))
         (begin (access-begin range))
         (end (access-end range))
         (keys (read-keys range)))
    (if (iterate
          (for i from begin below end)
          (for l = (aref keys i))
          (finding t such-that (funcall test l location)))
        (values result t)
        (values nil nil))))


(defun make-hash-table-range (hash-table)
  (let ((keys (coerce (hash-table-keys hash-table) 'vector)))
    (make-instance 'hash-table-range
                   :hash-table hash-table
                   :keys keys
                   :begin 0
                   :end (length keys))))
