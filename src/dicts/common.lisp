(in-package #:cl-ds.dicts)


(defclass content-tuple ()
  ((%location
    :initarg :location
    :accessor access-location)
   (%value
    :initarg :value
    :accessor access-value)))


(defclass hash-content-tuple (content-tuple)
  ((%hash
    :type fixnum
    :initform 0
    :initarg :hash
    :accessor access-hash)))


(defclass bucket ()
  ((%content
    :type list
    :initform (list)
    :initarg :content
    :accessor access-content)))


(defgeneric single-elementp (bucket)
  (:method ((bucket bucket))
    (endp (rest (access-content bucket)))))


(defgeneric find-content (container bucket location &key &allow-other-keys))


(defmethod find-content ((container hashing-dictionary) (bucket bucket) location &key hash)
  (let ((equal-fn (read-equal-fn container)))
    (flet ((compare-fn (a b)
             (and (eql (access-hash a) hash)
                  (funcall equal-fn
                           (access-location a)
                           b))))
      (declare (dynamic-extent (function compare-fn)))
      (multiple-value-bind (r f) (try-find
                                  location
                                  (access-content bucket)
                                  :test #'compare-fn)
        (values (and f (access-value r))
                f)))))


(defmethod cl-ds:grow-bucket ((operation cl-ds:insert-function)
                              (container hashing-dictionary)
                              (bucket bucket)
                              location
                              &key hash value &allow-other-keys)
  (let ((equal-fn (read-equal-fn container)))
    (flet ((compare-fn (a b)
             (and (eql (access-hash a) (access-hash b))
                  (funcall equal-fn
                           (access-location a)
                           (access-location b)))))
      (declare (dynamic-extent (function compare-fn)))
      (multiple-value-bind (next-list replaced old-value)
          (insert-or-replace (access-content bucket)
                             (make 'hash-content-tuple
                                   :hash hash
                                   :location location
                                   :value value)
                             :test #'compare-fn)
        (values (make 'bucket :content next-list)
                (cl-ds.common:make-eager-modification-operation-status
                 replaced
                 (and replaced (access-value old-value)))
                t)))))


(defmethod cl-ds:grow-bucket ((operation cl-ds:add-function)
                              (container hashing-dictionary)
                              (bucket bucket)
                              location
                              &key hash value &allow-other-keys)
  (let ((equal-fn (read-equal-fn container)))
    (flet ((compare-fn (a b)
             (and (eql (access-hash a) (access-hash b))
                  (funcall equal-fn
                           (access-location a)
                           (access-location b)))))
      (declare (dynamic-extent (function compare-fn)))
      (multiple-value-bind (next-list replaced old-value)
          (insert-or-replace (access-content bucket)
                             (make 'hash-content-tuple
                                   :hash hash
                                   :location location
                                   :value value)
                             :test #'compare-fn)
        (values (if replaced bucket (make 'bucket :content next-list))
                (cl-ds.common:make-eager-modification-operation-status
                 replaced
                 (and replaced (access-value old-value)))
                (not replaced))))))


(defmethod cl-ds:grow-bucket ((operation cl-ds:update-function)
                              (container hashing-dictionary)
                              (bucket bucket)
                              location
                              &key hash value &allow-other-keys)
  (let ((equal-fn (read-equal-fn container)))
    (flet ((compare-fn (a b)
             (and (eql (access-hash a) (access-hash b))
                  (funcall equal-fn
                           (access-location a)
                           (access-location b)))))
      (declare (dynamic-extent (function compare-fn)))
      (multiple-value-bind (next-list replaced old-value)
          (insert-or-replace (access-content bucket)
                             (make 'hash-content-tuple
                                   :hash hash
                                   :location location
                                   :value value)
                             :test #'compare-fn)
        (values (if replaced (make 'bucket :content next-list) bucket) 
                (cl-ds.common:make-eager-modification-operation-status
                 replaced
                 (and replaced (access-value old-value)))
                replaced)))))


(defmethod cl-ds:make-bucket ((operation cl-ds:update-function)
                              (container hashing-dictionary)
                              location
                              &key hash value &allow-other-keys)
  (values nil
          cl-ds.common:empty-eager-modification-operation-status
          nil))


(defmethod cl-ds:make-bucket ((operation cl-ds:add-function)
                              (container hashing-dictionary)
                              location &key hash value &allow-other-keys)
  (values (make 'bucket
                :content (list (make 'hash-content-tuple
                                     :location location
                                     :value value
                                     :hash hash)))
          cl-ds.common:empty-eager-modification-operation-status
          t))


(defmethod cl-ds:make-bucket ((operation cl-ds:insert-function)
                              (container hashing-dictionary)
                              location &key hash value &allow-other-keys)
  (values (make 'bucket
                :content (list (make 'hash-content-tuple
                                     :location location
                                     :value value
                                     :hash hash)))
          cl-ds.common:empty-eager-modification-operation-status
          t))
