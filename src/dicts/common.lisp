(in-package #:cl-ds.dicts)


(defstruct content-tuple location value)


(defstruct (hash-content-tuple (:include content-tuple))
  (hash 0 :type fixnum))


(deftype bucket () 'list)


(defgeneric single-elementp (bucket)
  (:method ((bucket list))
    (endp (rest bucket))))


(defgeneric find-content (container bucket location &key &allow-other-keys))


(defmethod find-content ((container hashing-dictionary)
                         (bucket list) location &key hash)
  (let ((equal-fn (read-equal-fn container)))
    (flet ((compare-fn (a b)
             (and (eql (hash-content-tuple-hash a) hash)
                  (funcall equal-fn
                           (content-tuple-location a)
                           b))))
      (declare (dynamic-extent (function compare-fn)))
      (multiple-value-bind (r f) (try-find
                                  location
                                  bucket
                                  :test #'compare-fn)
        (values (and f (content-tuple-value r))
                f)))))


(defmacro bucket-growing-macro ((container bucket location hash value)
                                result status changed)
  (with-gensyms (!equal-fn !compare-fn)
    (once-only (container bucket location hash value)
      `(let ((,!equal-fn (read-equal-fn ,container)))
         (flet ((,!compare-fn (a b)
                  (and (eql (hash-content-tuple-hash a) (hash-content-tuple-hash b))
                       (funcall ,!equal-fn
                                (content-tuple-location a)
                                (content-tuple-location b)))))
           (declare (dynamic-extent (function ,!compare-fn)))
           (multiple-value-bind (^next-list ^replaced ^old-value)
               (insert-or-replace ,bucket
                                  (make-hash-content-tuple
                                   :hash ,hash
                                   :location ,location
                                   :value ,value)
                                  :test (function ,!compare-fn))
             (values ,result
                     ,status
                     ,changed)))))))


(defmethod cl-ds:shrink-bucket ((operation cl-ds:erase-function)
                                (container hashing-dictionary)
                                (bucket list)
                                location
                                &key hash)
  (let ((equal-fn (read-equal-fn container)))
    (flet ((location-test (node location)
             (and (eql hash (hash-content-tuple-hash node))
                  (funcall equal-fn location
                           (hash-content-tuple-location node)))))
      (declare (dynamic-extent (function location-test)))
      (multiple-value-bind (list removed value)
          (try-remove location
                      bucket
                      :test #'location-test)
        (if removed
            (values
             list
             (cl-ds.common:make-eager-modification-operation-status
              t
              (content-tuple-value value))
             t)
            (values
             bucket
             cl-ds.common:empty-eager-modification-operation-status
             nil))))))


(defmethod cl-ds:grow-bucket ((operation cl-ds:insert-function)
                              (container hashing-dictionary)
                              (bucket list)
                              location
                              &key hash value)
  (bucket-growing-macro
      (container bucket location hash value)

      ^next-list
      (cl-ds.common:make-eager-modification-operation-status
       ^replaced
       (and ^replaced (content-tuple-value ^old-value)))
      t))


(defmethod cl-ds:grow-bucket ((operation cl-ds:functional-add-function)
                              (container hashing-dictionary)
                              (bucket list)
                              location
                              &key hash value)
  (bucket-growing-macro
      (container bucket location hash value)

      (if ^replaced bucket ^next-list)
      (cl-ds.common:make-eager-modification-operation-status
       ^replaced
       (and ^replaced (content-tuple-value ^old-value)))
      (not ^replaced)))


(defmethod cl-ds:grow-bucket ((operation cl-ds:functional-update-function)
                              (container hashing-dictionary)
                              (bucket list)
                              location
                              &key hash value)
  (bucket-growing-macro
      (container bucket location hash value)

      (if ^replaced ^next-list bucket)
      (if ^replaced
          (cl-ds.common:make-eager-modification-operation-status
           ^replaced (content-tuple-value ^old-value))
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
  (values (list (make-hash-content-tuple
                      :location location
                      :value value
                      :hash hash))
          cl-ds.common:empty-eager-modification-operation-status
          t))


(defmethod cl-ds:make-bucket ((operation cl-ds:insert-function)
                              (container hashing-dictionary)
                              location &key hash value)
  (values (list (make-hash-content-tuple
                      :location location
                      :value value
                      :hash hash))
          cl-ds.common:empty-eager-modification-operation-status
          t))


(flet ((locate-tuple (container bucket hash location)
         (declare (type hashing-dictionary container)
                  (type bucket bucket)
                  (type fixnum hash))
         (fbind ((comp (read-equal-fn container)))
           (iterate
             (for tuple in bucket)
             (finding
              tuple
              such-that (and (eql (hash-content-tuple-hash tuple)
                                  hash)
                             (comp (content-tuple-location tuple)
                                   location)))))))

  (defmethod cl-ds:grow-bucket! ((operation cl-ds:insert!-function)
                                 (container hashing-dictionary)
                                 (bucket list)
                                 location &key hash value)
    (let* ((tuple (locate-tuple container bucket hash location))
           (old-value (and tuple (content-tuple-value tuple))))
      (if (null tuple)
          (push (make-hash-content-tuple
                      :location location
                      :value value
                      :hash hash)
                bucket)
          (setf (hash-content-tuple-hash tuple) hash
                (content-tuple-value tuple) value))
      (values bucket
              (if (null tuple)
                  cl-ds.common:empty-eager-modification-operation-status
                  (cl-ds.common:make-eager-modification-operation-status
                   t old-value))
              t)))

  (defmethod cl-ds:grow-bucket! ((operation cl-ds:update!-function)
                                 (container hashing-dictionary)
                                 (bucket list)
                                 location &key hash value)
    (let* ((tuple (locate-tuple container bucket hash location))
           (old-value (and tuple (content-tuple-value tuple))))
      (if (null tuple)
          (values bucket
                  cl-ds.common:empty-eager-modification-operation-status
                  nil)
          (progn
            (setf (hash-content-tuple-hash tuple) hash
                  (content-tuple-value tuple) value)
            (values bucket
                    (cl-ds.common:make-eager-modification-operation-status
                     t old-value)
                    t)))))

  (defmethod cl-ds:grow-bucket! ((operation cl-ds:add!-function)
                                 (container hashing-dictionary)
                                 (bucket list)
                                 location &key hash value)
    (let* ((tuple (locate-tuple container bucket hash location))
           (old-value (and tuple (content-tuple-value tuple))))
      (if (null tuple)
          (progn
            (push (make-hash-content-tuple
                        :location location
                        :value value
                        :hash hash)
                  bucket)
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
                                 (bucket list)
                                 location &key hash)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0))
           (type fixnum hash))
  (fbind ((comp (read-equal-fn container)))
    (iterate
      (for cell on bucket)
      (for p-cell previous cell)
      (for tuple = (first cell))
      (when (and (eql (hash-content-tuple-hash tuple) hash)
                 (comp (hash-content-tuple-location tuple)
                       location))
        (if (null p-cell)
            (setf bucket (rest cell))
            (setf (cdr p-cell) (cdr cell)))
        (return-from cl-ds:shrink-bucket!
          (values bucket
                  (cl-ds.common:make-eager-modification-operation-status
                   t
                   (hash-content-tuple-value tuple))
                  t)))
      (finally
       (return (values bucket
                       cl-ds.common:empty-eager-modification-operation-status
                       nil))))))
