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


(defmethod find-content ((container hashing-dictionary)
                         (bucket bucket) location &key hash)
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


(defmacro bucket-growing-macro ((container bucket location hash value)
                                result status changed)
  (with-gensyms (!equal-fn !compare-fn)
    (once-only (container bucket location hash value)
      `(let ((,!equal-fn (read-equal-fn ,container)))
         (flet ((,!compare-fn (a b)
                  (and (eql (access-hash a) (access-hash b))
                       (funcall ,!equal-fn
                                (access-location a)
                                (access-location b)))))
           (declare (dynamic-extent (function ,!compare-fn)))
           (multiple-value-bind (^next-list ^replaced ^old-value)
               (insert-or-replace (access-content ,bucket)
                                  (make 'hash-content-tuple
                                        :hash ,hash
                                        :location ,location
                                        :value ,value)
                                  :test (function ,!compare-fn))
             (values ,result
                     ,status
                     ,changed)))))))


(defmethod cl-ds:shrink-bucket ((operation cl-ds:erase-function)
                                (container hashing-dictionary)
                                (bucket bucket)
                                location
                                &key hash)
  (let ((equal-fn (read-equal-fn container)))
    (flet ((location-test (node location)
             (and (eql hash (access-hash node))
                  (funcall equal-fn location
                           (access-location node)))))
      (declare (dynamic-extent (function location-test)))
      (multiple-value-bind (list removed value)
          (try-remove location
                      (access-content bucket)
                      :test #'location-test)
        (if removed
            (values
             (and list (make 'bucket :content list))
             (cl-ds.common:make-eager-modification-operation-status
              t
              (access-value value))
             t)
            (values
             bucket
             cl-ds.common:empty-eager-modification-operation-status
             nil))))))


(defmethod cl-ds:grow-bucket ((operation cl-ds:insert-function)
                              (container hashing-dictionary)
                              (bucket bucket)
                              location
                              &key hash value)
  (bucket-growing-macro
      (container bucket location hash value)

      (make 'bucket :content ^next-list)
      (cl-ds.common:make-eager-modification-operation-status
       ^replaced
       (and ^replaced (access-value ^old-value)))
      t))


(defmethod cl-ds:grow-bucket ((operation cl-ds:functional-add-function)
                              (container hashing-dictionary)
                              (bucket bucket)
                              location
                              &key hash value)
  (bucket-growing-macro
      (container bucket location hash value)

      (if ^replaced bucket (make 'bucket :content ^next-list))
      (cl-ds.common:make-eager-modification-operation-status
       ^replaced
       (and ^replaced (access-value ^old-value)))
      (not ^replaced)))


(defmethod cl-ds:grow-bucket ((operation cl-ds:functional-update-function)
                              (container hashing-dictionary)
                              (bucket bucket)
                              location
                              &key hash value)
  (bucket-growing-macro
      (container bucket location hash value)

      (if ^replaced (make 'bucket :content ^next-list) bucket)
      (if ^replaced
          (cl-ds.common:make-eager-modification-operation-status
           ^replaced (access-value ^old-value))
          cl-ds.common:empty-eager-modification-operation-status)
      ^replaced))


(defmethod cl-ds:make-bucket ((operation cl-ds:functional-update-function)
                              (container hashing-dictionary)
                              location
                              &key hash value)
  (declare (ignore hash value location))
  (values nil
          cl-ds.common:empty-eager-modification-operation-status
          nil))


(defmethod cl-ds:make-bucket ((operation cl-ds:add-function)
                              (container hashing-dictionary)
                              location &key hash value) 
  (values (make 'bucket
                :content (list (make 'hash-content-tuple
                                     :location location
                                     :value value
                                     :hash hash)))
          cl-ds.common:empty-eager-modification-operation-status
          t))


(defmethod cl-ds:make-bucket ((operation cl-ds:insert-function)
                              (container hashing-dictionary)
                              location &key hash value)
  (values (make 'bucket
                :content (list (make 'hash-content-tuple
                                     :location location
                                     :value value
                                     :hash hash)))
          cl-ds.common:empty-eager-modification-operation-status
          t))


(flet ((locate-tuple (container bucket hash location)
         (declare (type hashing-dictionary container)
                  (type bucket bucket)
                  (type fixnum hash))
         (fbind ((comp (read-equal-fn container)))
           (iterate
             (for tuple in (access-content bucket))
             (finding
              tuple
              such-that (and (eql (access-hash tuple)
                                  hash)
                             (comp (access-location tuple)
                                   location)))))))

  (defmethod cl-ds:grow-bucket! ((operation cl-ds:insert!-function)
                                 (container hashing-dictionary)
                                 (bucket bucket)
                                 location &key hash value)
    (let* ((tuple (locate-tuple container bucket hash location))
           (old-value (and tuple (access-value tuple))))
      (if (null tuple)
          (push (make 'hash-content-tuple
                      :location location
                      :value value
                      :hash hash)
                (access-content bucket))
          (setf (access-hash tuple) hash
                (access-value tuple) value))
      (values bucket
              (if (null tuple)
                  cl-ds.common:empty-eager-modification-operation-status
                  (cl-ds.common:make-eager-modification-operation-status
                   t old-value))
              t)))

  (defmethod cl-ds:grow-bucket! ((operation cl-ds:update!-function)
                                 (container hashing-dictionary)
                                 (bucket bucket)
                                 location &key hash value)
    (let* ((tuple (locate-tuple container bucket hash location))
           (old-value (and tuple (access-value tuple))))
      (if (null tuple)
          (values bucket
                  cl-ds.common:empty-eager-modification-operation-status
                  nil)
          (progn
            (setf (access-hash tuple) hash
                  (access-value tuple) value)
            (values bucket
                    (cl-ds.common:make-eager-modification-operation-status
                     t old-value)
                    t)))))

  (defmethod cl-ds:grow-bucket! ((operation cl-ds:add!-function)
                                 (container hashing-dictionary)
                                 (bucket bucket)
                                 location &key hash value)
    (let* ((tuple (locate-tuple container bucket hash location))
           (old-value (and tuple (access-value tuple))))
      (if (null tuple)
          (progn
            (push (make 'hash-content-tuple
                        :location location
                        :value value
                        :hash hash)
                  (access-content bucket))
            (values bucket
                    cl-ds.common:empty-eager-modification-operation-status
                    t))
          (values bucket
                  (cl-ds.common:make-eager-modification-operation-status
                   t
                   old-value)
                  nil)))))


(defmethod cl-ds:shrink-bucket! ((operation cl-ds:erase!-function)
                                 (container hashing-dictionary)
                                 (bucket bucket)
                                 location &key hash)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0))
           (type fixnum hash))
  (fbind ((comp (read-equal-fn container)))
    (iterate
      (for cell on (access-content bucket))
      (for p-cell previous cell)
      (for tuple = (car cell))
      (when (and (eql (access-hash tuple) hash)
                 (comp (access-location tuple)
                       location))
        (if (null p-cell)
            (setf (access-content bucket)
                  (cdr cell))
            (setf (cdr p-cell) (cdr cell)))
        (return-from cl-ds:shrink-bucket!
          (values (and (access-content bucket) bucket)
                  (cl-ds.common:make-eager-modification-operation-status
                   t
                   (access-value tuple))
                  t)))
      (finally
       (return (values bucket
                       cl-ds.common:empty-eager-modification-operation-status
                       nil))))))
