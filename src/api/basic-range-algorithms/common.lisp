(in-package #:cl-data-structures)


(defclass proxy-range ()
  ((%original-range :initarg :original-range
                    :reader read-original-range)))


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


(defclass hash-table-range (fundamental-random-access-range)
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


(defmethod consume-front ((range forward-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod consume-back ((range bidirectional-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod peek-front ((range forward-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod peek-back ((range bidirectional-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod at ((range random-access-proxy-box-range) location)
  (funcall (read-function range) (call-next-method)))


(defmethod consume-front ((range hash-table-range))
  (with-slots ((begin %begin) (end %end) (ht %hash-table) (keys %keys)) range
    (if (eql begin end)
        (values nil nil)
        (let ((key (aref keys begin)))
          (incf begin)
          (values
           (list* key
                  (gethash key ht))
           t)))))


(defmethod consume-back ((range hash-table-range))
  (with-slots ((begin %begin) (end %end) (ht %hash-table) (keys %keys)) range
    (if (eql begin end)
        (values nil nil)
        (let ((key (aref keys (decf end))))
          (values
           (list* key
                  (gethash key ht))
           t)))))


(defmethod peek-front ((range hash-table-range))
  (with-slots ((begin %begin) (end %end) (ht %hash-table) (keys %keys)) range
    (if (eql begin end)
        (values nil nil)
        (let ((key (aref keys begin)))
          (values
           (list* key
                  (gethash key ht))
           t)))))


(defmethod peek-back ((range hash-table-range))
  (with-slots ((begin %begin) (end %end) (ht %hash-table) (keys %keys)) range
    (if (eql begin end)
        (values nil nil)
        (let ((key (aref keys end)))
          (values
           (list* key
                  (gethash key ht))
           t)))))


(defmethod morep ((range hash-table-range))
  (with-slots ((begin %begin) (end %end)) range
    (not (eql begin end))))


(defmethod at ((range hash-table-range) location)
  (with-slots ((ht %hash-table)) range
    (let ((test (hash-table-test ht))
          (begin (access-begin range))
          (end (access-end range))
          (keys (read-keys range)))
      (if (iterate
            (for i from begin below end)
            (for l = (aref keys i))
            (finding t such-that (funcall test l location)))
          (gethash location ht)
          (values nil nil)))))


(defun make-hash-table-range (hash-table)
  (let ((keys (coerce (hash-table-keys hash-table) 'vector)))
    (make-instance 'hash-table-range
                   :hash-table hash-table
                   :keys keys
                   :begin 0
                   :end (length keys))))
