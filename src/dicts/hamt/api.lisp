(in-package #:cl-ds.dicts.hamt)


(defclass functional-hamt-dictionary (hamt-dictionary
                                      cl-ds:functional)
  ())


(defclass mutable-hamt-dictionary (hamt-dictionary
                                   cl-ds:mutable)
  ())


(defclass transactional-hamt-dictionary (hamt-dictionary
                                         cl-ds:transactional)
  ((%root-was-modified :type boolean
                       :initform nil
                       :accessor access-root-was-modified
                       :initarg :root-was-modified)))


(-> make-functional-hamt-dictionary ((-> (t) fixnum)
                                     (-> (t t) boolean)
                                     &key (:max-depth positive-fixnum))
    functional-hamt-dictionary)
(defun make-functional-hamt-dictionary (hash-fn equal-fn &key (max-depth +depth+))
  (declare (optimize (safety 3)))
  (unless (< 0 max-depth (1+ +depth+))
    (error
     'cl-ds:initialization-out-of-bounds
     :argument :max-depth
     :value max-depth
     :bounds (list 0 (1+ +depth+))
     :class 'functional-hamt-dictionary
     :references '((:make-functional-hamt-dictionary "MAX-DEPTH"))))
  (assure functional-hamt-dictionary (make 'functional-hamt-dictionary
                                           :hash-fn hash-fn
                                           :root nil
                                           :max-depth max-depth
                                           :equal-fn equal-fn)))


(-> make-mutable-hamt-dictionary ((-> (t) fixnum)
                                  (-> (t t) boolean)
                                  &key (:max-depth positive-fixnum))
    mutable-hamt-dictionary)
(defun make-mutable-hamt-dictionary (hash-fn equal-fn &key (max-depth +depth+))
  (declare (optimize (safety 3)))
  (unless (< 0 max-depth (1+ +depth+))
    (error
     'cl-ds:initialization-out-of-bounds
     :argument :max-depth
     :value max-depth
     :bounds (list 0 (1+ +depth+))
     :class 'mutable-hamt-dictionary
     :references '((:make-mutable-hamt-dictionary "MAX-DEPTH"))))
  (assure mutable-hamt-dictionary (make 'mutable-hamt-dictionary
                                        :equal-fn equal-fn
                                        :hash-fn hash-fn
                                        :root nil
                                        :max-depth max-depth)))


(-> hamt-dictionary-at (hamt-dictionary t) (values t boolean))
(defun hamt-dictionary-at (container location)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "Implementation of AT"
  (with-hash-tree-functions (container)
    (let* ((hash (hash-fn location))
           (root (access-root container)))
      (declare (type fixnum hash))
      (flet ((location-test (loc location)
               (and (eql hash (hash.location.value-hash loc))
                    (equal-fn location
                              (hash.location.value-location loc)))))
        (declare (dynamic-extent (function location-test)))
        (hash-do
            (node index)
            (root hash)
            :on-leaf (cl-ds.dicts:find-content container
                                               node
                                               location
                                               :hash hash)
            :on-nil (values nil nil))))))


(-> hamt-dictionary-size (hamt-dictionary) non-negative-fixnum)
(defun hamt-dictionary-size (container)
  "Implementation of SIZE"
  (access-size container))


(-> mutable-hamt-dictionary-erase! (mutable-hamt-dictionary t)
    (values mutable-hamt-dictionary
            cl-ds:fundamental-modification-operation-status))
(defun mutable-hamt-dictionary-erase! (container location)
  (declare (optimize (speed 3)))
  "Implementation of ERASE!"
  (with-hash-tree-functions (container)
    (let ((old-value nil)
          (hash (hash-fn location)))
      (flet ((location-test (loc location)
               (and (eql hash (hash.location.value-hash loc))
                    (equal-fn location (hash.location.value-location loc)))))
        (let ((new-root
                (with-destructive-erase-hamt node container (hash-fn location)
                  :on-leaf
                  (multiple-value-bind (next found value)
                      (try-remove location
                                  (access-conflict node)
                                  :test #'location-test)
                    (if found
                        (progn
                          (setf old-value
                                (hash.location.value-value value))
                          (when next
                            (setf (access-conflict node) next)
                            node))
                        (return-from mutable-hamt-dictionary-erase!
                          (values
                           container
                           cl-ds.common:empty-eager-modification-operation-status))))
                  :on-nil
                  (return-from mutable-hamt-dictionary-erase!
                    (values
                     container
                     cl-ds.common:empty-eager-modification-operation-status)))))
          (decf (access-size container))
          (setf (access-root container) new-root)
          (values container
                  (cl-ds.common:make-eager-modification-operation-status
                   t old-value)))))))


#|

Methods. Those will just call non generic functions.

|#


(defmethod cl-ds:erase! ((container mutable-hamt-dictionary) location)
  (mutable-hamt-dictionary-erase! container location))


(defmethod cl-ds:size ((container hamt-dictionary))
  (hamt-dictionary-size container))


(defmethod cl-ds:at ((container hamt-dictionary) location)
  (hamt-dictionary-at container location))


(defmethod cl-ds:position-modification ((operation cl-ds:grow-function)
                                        (container functional-hamt-dictionary)
                                        location &key value)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (container :cases nil)
    (let ((hash (hash-fn location))
          (changed nil))
      (flet ((grow-bucket (bucket)
               (multiple-value-bind (a b c)
                   (cl-ds:grow-bucket operation container bucket location :value value :hash hash)
                 (setf changed c)
                 (values a b c)))
             (make-bucket ()
               (multiple-value-bind (a b c)
                   (cl-ds:make-bucket operation container location :value value :hash hash)
                 (setf changed c)
                 (values a b c))))
        (declare (dynamic-extent (function make-bucket) (function grow-bucket)))
        (multiple-value-bind (new-root status)
            (go-down-on-path container
                             hash
                             #'grow-bucket
                             #'make-bucket
                             #'copy-on-write)
          (values (if changed
                      (make 'functional-hamt-dictionary
                            :hash-fn (cl-ds.dicts:read-hash-fn container)
                            :equal-fn (cl-ds.dicts:read-equal-fn container)
                            :root new-root
                            :max-depth (read-max-depth container)
                            :size (if (cl-ds:found status)
                                      (the non-negative-fixnum (access-size container))
                                      (1+ (the non-negative-fixnum (access-size container)))))
                      container)
                  status))))))


(defmethod cl-ds:position-modification ((operation cl-ds:grow-function)
                                        (container transactional-hamt-dictionary)
                                        location &key value)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (container :cases nil)
    (let ((hash (hash-fn location))
          (changed nil))
      (flet ((grow-bucket (bucket)
               (multiple-value-bind (a b c)
                   (cl-ds:grow-bucket operation container bucket location :value value :hash hash)
                 (setf changed c)
                 (values a b c)))
             (make-bucket ()
               (multiple-value-bind (a b c)
                   (cl-ds:make-bucket operation container location :value value :hash hash)
                 (setf changed c)
                 (values a b c)))
             (copy-on-write (indexes path depth max-depth conflict)
               (transactional-copy-on-write indexes
                                            path
                                            depth
                                            max-depth
                                            conflict
                                            (the boolean (access-root-was-modified container)))))
        (declare (dynamic-extent (function make-bucket)
                                 (function grow-bucket)
                                 (function copy-on-write)))
        (multiple-value-bind (new-root status)
            (go-down-on-path container
                             hash
                             #'grow-bucket
                             #'make-bucket
                             #'copy-on-write)
          (when changed
            (setf (access-root container) new-root
                  (access-root-was-modified container) t)
            (unless (cl-ds:found status)
              (incf (the non-negative-fixnum (access-size container)))))
          (values container
                  status))))))


(defmethod cl-ds:position-modification ((operation cl-ds:shrink-function)
                                        (container functional-hamt-dictionary)
                                        location &key)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (container :cases nil)
    (let ((hash (hash-fn location))
          (changed nil))
      (flet ((shrink-bucket (bucket)
               (multiple-value-bind (a b c)
                   (cl-ds:shrink-bucket operation container bucket location :hash hash)
                 (setf changed c)
                 (values a b c)))
             (just-return ()
               (return-from cl-ds:position-modification
                 (values container
                         cl-ds.common:empty-eager-modification-operation-status))))
        (declare (dynamic-extent (function just-return) (function shrink-bucket)))
        (multiple-value-bind (new-root status)
            (go-down-on-path container
                             hash
                             #'shrink-bucket
                             #'just-return
                             #'copy-on-write)
          (values (if changed
                      (make 'functional-hamt-dictionary
                            :hash-fn (cl-ds.dicts:read-hash-fn container)
                            :equal-fn (cl-ds.dicts:read-equal-fn container)
                            :root new-root
                            :max-depth (read-max-depth container)
                            :size (1- (the non-negative-fixnum (access-size container))))
                      container)
                  status))))))


(defmethod cl-ds:position-modification ((operation cl-ds:shrink-function)
                                        (container transactional-hamt-dictionary)
                                        location &key)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (container :cases nil)
    (let ((hash (hash-fn location))
          (changed nil))
      (flet ((shrink-bucket (bucket)
               (multiple-value-bind (a b c)
                   (cl-ds:shrink-bucket operation container bucket location :hash hash)
                 (setf changed c)
                 (values a b c)))
             (just-return ()
               (return-from cl-ds:position-modification
                 (values container
                         cl-ds.common:empty-eager-modification-operation-status)))
             (copy-on-write (indexes path depth max-depth conflict)
               (transactional-copy-on-write indexes
                                            path
                                            depth
                                            max-depth
                                            conflict
                                            (access-root-was-modified container))))
        (declare (dynamic-extent (function just-return)
                                 (function shrink-bucket)
                                 (function copy-on-write)))
        (multiple-value-bind (new-root status)
            (go-down-on-path container
                             hash
                             #'shrink-bucket
                             #'just-return
                             #'copy-on-write)
          (when changed
            (setf (access-root container) new-root)
            (decf (the non-negative-fixnum (access-size container))))
          (values container
                  status))))))


(defmethod cl-ds:position-modification ((operation cl-ds:grow-function)
                                        (container mutable-hamt-dictionary)
                                        location &key value)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0)))
  (let ((status nil)
        (hash (funcall (the (-> (t) fixnum)
                            (cl-ds.dicts:read-hash-fn container)) location)))
    (macrolet ((handle-bucket (&body body)
                 `(multiple-value-bind (bucket s changed)
                      ,@body
                    (unless changed
                      (return-from cl-ds:position-modification
                        (values container
                                s)))
                    (setf status s)
                    bucket)))
      (let* ((prev-node nil)
             (prev-index 0)
             (root (access-root container))
             (result
               (hash-do
                   (node index c)
                   ((access-root container) hash)
                   :on-every (setf prev-node node prev-index index)
                   :on-nil (if prev-node
                                 (progn
                                   (assert (not (hash-node-contains-leaf
                                                 prev-node prev-index)))
                                   (hash-node-insert!
                                    prev-node
                                    (rebuild-rehashed-node
                                     c
                                     (read-max-depth container)
                                     (handle-bucket
                                      (cl-ds:make-bucket operation
                                                         container
                                                         location
                                                         :hash hash
                                                         :value value)))
                                    prev-index)
                                   root)
                                 (handle-bucket
                                  (cl-ds:make-bucket operation
                                                     container
                                                     location
                                                     :hash hash
                                                     :value value)))
                   :on-leaf (if prev-node
                                  (progn
                                    (assert (hash-node-contains-leaf
                                             prev-node prev-index))
                                    (hash-node-replace!
                                     prev-node
                                     (rebuild-rehashed-node
                                      c
                                      (read-max-depth container)
                                      (handle-bucket
                                       (cl-ds:grow-bucket! operation
                                                           container
                                                           node
                                                           location
                                                           :hash hash
                                                           :value value)))
                                     prev-index)
                                    root)
                                  (rebuild-rehashed-node
                                   c
                                   (read-max-depth container)
                                   (handle-bucket
                                    (cl-ds:grow-bucket! operation
                                                        container
                                                        node
                                                        location
                                                        :hash hash
                                                        :value value)))))))
        (setf (access-root container) result)
        (unless (cl-ds:found status)
          (incf (the fixnum (access-size container))))
        (values container
                status)))))


(defmethod cl-ds:become-mutable ((container functional-hamt-dictionary))
  (make 'mutable-hamt-dictionary
        :hash-fn (read-hash-fn container)
        :root (access-root container)
        :max-depth (read-max-depth container)
        :equal-fn (read-equal-fn container)
        :size (access-size container)))


(defmethod cl-ds:become-functional ((container functional-hamt-dictionary))
  (make 'functional-hamt-dictionary
        :hash-fn (read-hash-fn container)
        :root (access-root container)
        :max-depth (read-max-depth container)
        :equal-fn (read-equal-fn container)
        :size (access-size container)))


(defmethod cl-ds:become-transactional ((container hamt-dictionary))
  (make 'transactional-hamt-dictionary
        :hash-fn (read-hash-fn container)
        :root (access-root container)
        :max-depth (read-max-depth container)
        :equal-fn (read-equal-fn container)
        :size (access-size container)))


(defmethod cl-ds:become-transactional ((container transactional-hamt-dictionary))
  (let ((root (access-root container)))
    (when (and root (hash-node-p root))
      (setf root (isolate-transactional-instance
                  root
                  (access-root-was-modified container))))
    (make 'transactional-hamt-dictionary
          :hash-fn (read-hash-fn container)
          :root root
          :max-depth (read-max-depth container)
          :equal-fn (read-equal-fn container)
          :root-was-modified (access-root-was-modified container)
          :size (access-size container))))


(defmethod cl-ds:become-mutable ((container transactional-hamt-dictionary))
  (let ((root (access-root container)))
    (when (and root (hash-node-p root))
      (clear-modification-masks root))
    (make 'mutable-hamt-dictionary
          :hash-fn (read-hash-fn container)
          :root root
          :max-depth (read-max-depth container)
          :equal-fn (read-equal-fn container)
          :size (access-size container))))


(defmethod cl-ds:become-functional ((container transactional-hamt-dictionary))
  (let ((root (access-root container)))
    (when (and root (hash-node-p root))
      (clear-modification-masks root))
    (make 'functional-hamt-dictionary
          :hash-fn (read-hash-fn container)
          :root root
          :max-depth (read-max-depth container)
          :equal-fn (read-equal-fn container)
          :size (access-size container))))


(defmethod alexandria:emptyp ((container hamt-dictionary))
  (null (access-root container)))
