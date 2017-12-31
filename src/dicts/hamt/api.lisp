(in-package #:cl-ds.dicts.hamt)


(defclass hamt-dictionary (cl-ds.common.hamt:hamt-container
                           cl-ds.dicts:abstract-hashing-dictionary)
  ())


(defmethod cl-ds:empty-clone ((container hamt-dictionary))
  (make (type-of container)
        :root nil
        :size 0
        :hash-fn (cl-ds.dicts:read-hash-fn container)))


(defclass functional-hamt-dictionary (cl-ds.dicts:functional-hashing-dictionary
                                      hamt-dictionary)
  ())


(defclass mutable-hamt-dictionary (cl-ds.dicts:mutable-hashing-dictionary
                                   hamt-dictionary)
  ())


(defmacro with-hash-tree-functions ((container &key (cases nil)) &body body)
  "Simple macro adding local functions (all forwards to the container closures)."
  (once-only (container)
    (with-gensyms (!test !hash)
      `(let ((,!test (cl-ds.dicts:read-equal-fn ,container))
             (,!hash (cl-ds.dicts:read-hash-fn ,container)))
         (nest
          ,(if cases
               `(cl-ds.utils:cases
                    ((eq ,!test #'eql)
                     (eq ,!test #'equal)
                     (eq ,!test #'string=)
                     (eq ,!test #'eq)))
               `(progn))
          (flet ((equal-fn (a b)
                   (funcall ,!test a b))
                 (hash-fn (x)
                   (funcall ,!hash x)))
            (declare (ignorable (function hash-fn)
                                (function equal-fn))
                     (inline hash-fn
                             equal-fn)
                     (dynamic-extent (function hash-fn)
                                     (function equal-fn)))
            ,@body))))))


(defclass transactional-hamt-dictionary (hamt-dictionary
                                         cl-ds.dicts:transactional-hashing-dictionary
                                         cl-ds.dicts:abstract-hashing-dictionary)
  ((%root-was-modified :type boolean
                       :initform nil
                       :accessor access-root-was-modified
                       :initarg :root-was-modified)))


(-> make-functional-hamt-dictionary ((-> (t) fixnum)
                                     (-> (t t) boolean))
    functional-hamt-dictionary)
(defun make-functional-hamt-dictionary (hash-fn equal-fn)
  (declare (optimize (safety 3)))
  (assure functional-hamt-dictionary (make 'functional-hamt-dictionary
                                           :hash-fn hash-fn
                                           :root nil
                                           :equal-fn equal-fn)))


(-> make-mutable-hamt-dictionary ((-> (t) fixnum)
                                  (-> (t t) boolean))
    mutable-hamt-dictionary)
(defun make-mutable-hamt-dictionary (hash-fn equal-fn)
  (declare (optimize (safety 3)))
  (assure mutable-hamt-dictionary (make 'mutable-hamt-dictionary
                                        :equal-fn equal-fn
                                        :hash-fn hash-fn
                                        :root nil)))


(-> hamt-dictionary-at (hamt-dictionary t) (values t boolean))
(defun hamt-dictionary-at (container location)
  (declare (optimize (speed 3) (safety 1) (debug 0) (space 0)))
  "Implementation of AT"
  (with-hash-tree-functions (container)
    (let* ((hash (hash-fn location))
           (root (access-root container)))
      (declare (type fixnum hash))
      (hash-do
          (node index)
          (root hash)
          :on-leaf (cl-ds.dicts:find-content container
                                             node
                                             location
                                             :hash hash)
          :on-nil (values nil nil)))))


(-> hamt-dictionary-size (hamt-dictionary) non-negative-fixnum)
(defun hamt-dictionary-size (container)
  "Implementation of SIZE"
  (access-size container))


#|

Methods. Those will just call non generic functions.

|#


(defmethod cl-ds:size ((container hamt-dictionary))
  (hamt-dictionary-size container))


(defmethod cl-ds:at ((container hamt-dictionary) location)
  (hamt-dictionary-at container location))


(defmethod cl-ds:position-modification ((operation cl-ds:grow-function)
                                        (container functional-hamt-dictionary)
                                        location &rest all &key value)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (container :cases nil)
    (let ((changed nil)
          (tag (list t))
          (hash (hash-fn location)))
      (flet ((grow-bucket (bucket)
               (multiple-value-bind (a b c)
                   (apply #'cl-ds:grow-bucket
                          operation
                          container
                          bucket
                          location
                          :hash hash
                          :value value
                          all)
                 (setf changed c)
                 (values a b c)))
             (copy-on-write (indexes path depth conflict)
               (copy-on-write tag
                              indexes
                              path
                              depth
                              conflict))
             (make-bucket ()
               (multiple-value-bind (a b c)
                   (apply #'cl-ds:make-bucket
                          operation
                          container
                          location
                          :hash hash
                          :value value
                          all)
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
                      (make
                       'functional-hamt-dictionary
                       :hash-fn (cl-ds.dicts:read-hash-fn container)
                       :equal-fn (cl-ds.dicts:read-equal-fn container)
                       :ownership-tag tag
                       :root new-root
                       :size (if (cl-ds:found status)
                                 (the non-negative-fixnum (access-size container))
                                 (1+ (the non-negative-fixnum (access-size container)))))
                      container)
                  status))))))


(defmethod cl-ds:position-modification ((operation cl-ds:grow-function)
                                        (container transactional-hamt-dictionary)
                                        location &rest all &key value)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (container :cases nil)
    (let ((hash (hash-fn location))
          (changed nil))
      (flet ((grow-bucket (bucket)
               (multiple-value-bind (a b c)
                   (apply #'cl-ds:grow-bucket
                          operation
                          container
                          bucket
                          location
                          :hash hash
                          :value value
                          all)
                 (setf changed c)
                 (values a b c)))
             (make-bucket ()
               (multiple-value-bind (a b c)
                   (apply #'cl-ds:make-bucket
                          operation
                          container
                          location
                          :hash hash
                          :value value
                          all)
                 (setf changed c)
                 (values a b c)))
             (copy-on-write (indexes path depth conflict)
               (transactional-copy-on-write (read-ownership-tag container)
                                            indexes
                                            path
                                            depth
                                            conflict)))
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
                                        location
                                        &rest all
                                        &key)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (container :cases nil)
    (let ((hash (hash-fn location))
          (tag (list t))
          (changed nil))
      (flet ((shrink-bucket (bucket)
               (multiple-value-bind (a b c)
                   (apply #'cl-ds:shrink-bucket
                          operation
                          container
                          bucket
                          location
                          :hash hash
                          all)
                 (setf changed c)
                 (values a b c)))
             (copy-on-write (indexes path depth conflict)
               (copy-on-write tag
                              indexes
                              path
                              depth
                              conflict))
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
                            :ownership-tag tag
                            :size (1- (the non-negative-fixnum (access-size container))))
                      container)
                  status))))))


(defmethod cl-ds:position-modification ((operation cl-ds:shrink-function)
                                        (container transactional-hamt-dictionary)
                                        location
                                        &rest all
                                        &key)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (container :cases nil)
    (let ((hash (hash-fn location))
          (changed nil))
      (declare (dynamic-extent hash changed))
      (flet ((shrink-bucket (bucket)
               (multiple-value-bind (a b c)
                   (apply #'cl-ds:shrink-bucket
                          operation
                          container
                          bucket
                          location :hash hash
                          all)
                 (setf changed c)
                 (values a b c)))
             (just-return ()
               (return-from cl-ds:position-modification
                 (values container
                         cl-ds.common:empty-eager-modification-operation-status)))
             (copy-on-write (indexes path depth conflict)
               (transactional-copy-on-write (read-ownership-tag container)
                                            indexes
                                            path
                                            depth
                                            conflict)))
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
            (setf (access-root container) new-root
                  (access-root-was-modified container) t)
            (decf (the non-negative-fixnum (access-size container))))
          (values container
                  status))))))


(defmethod cl-ds:position-modification ((operation cl-ds:shrink-function)
                                        (container mutable-hamt-dictionary)
                                        location
                                        &rest all
                                        &key)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (container)
    (let* ((modification-status nil)
           (hash (hash-fn location))
           (new-root
             (with-destructive-erase-hamt node container hash
               :on-leaf
               (multiple-value-bind (bucket status changed)
                   (apply #'cl-ds:shrink-bucket!
                          operation
                          container
                          node
                          location
                          :hash hash
                          all)
                 (unless changed
                   (return-from cl-ds:position-modification
                     (values container status)))
                 (setf modification-status status)
                 bucket)
               :on-nil
               (return-from cl-ds:position-modification
                 (values
                  container
                  cl-ds.common:empty-eager-modification-operation-status)))))
      (decf (access-size container))
      (setf (access-root container) new-root)
      (values container
              modification-status))))


(defmethod cl-ds:position-modification ((operation cl-ds:grow-function)
                                        (container mutable-hamt-dictionary)
                                        location
                                        &rest all
                                        &key value)
  (declare (optimize (speed 3)
                     (safety 1)
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
             (tag (read-ownership-tag container))
             (result
               (hash-do
                   (node index c)
                   ((access-root container) hash)
                   :on-every (setf prev-node node prev-index index)
                   :on-nil (if prev-node
                                 (progn
                                   (hash-node-insert!
                                    prev-node
                                    (rebuild-rehashed-node
                                     c
                                     (handle-bucket
                                      (cl-ds:make-bucket operation
                                                         container
                                                         location
                                                         :hash hash
                                                         :value value))
                                     tag)
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
                                  (hash-node-replace!
                                   prev-node
                                   (rebuild-rehashed-node c
                                    (handle-bucket
                                     (cl-ds:grow-bucket! operation
                                                         container
                                                         node
                                                         location
                                                         :hash hash
                                                         :value value))
                                    tag)
                                   prev-index)
                                  root)
                                (rebuild-rehashed-node
                                 c
                                 (handle-bucket
                                  (cl-ds:grow-bucket! operation
                                                      container
                                                      node
                                                      location
                                                      :hash hash
                                                      :value value))
                                 tag)))))
        (setf (access-root container) result)
        (unless (cl-ds:found status)
          (incf (the fixnum (access-size container))))
        (values container
                status)))))


(defmethod cl-ds:become-mutable ((container functional-hamt-dictionary))
  (make 'mutable-hamt-dictionary
        :hash-fn (cl-ds.dicts:read-hash-fn container)
        :root (access-root container)
        :equal-fn (cl-ds.dicts:read-equal-fn container)
        :size (access-size container)))


(defmethod cl-ds:become-functional ((container functional-hamt-dictionary))
  (make 'functional-hamt-dictionary
        :hash-fn (cl-ds.dicts:read-hash-fn container)
        :root (access-root container)
        :equal-fn (cl-ds.dicts:read-equal-fn container)
        :size (access-size container)))


(defmethod cl-ds:become-transactional ((container hamt-dictionary))
  (make 'transactional-hamt-dictionary
        :hash-fn (cl-ds.dicts:read-hash-fn container)
        :root (access-root container)
        :equal-fn (cl-ds.dicts:read-equal-fn container)
        :size (access-size container)))


(defmethod cl-ds:become-transactional ((container transactional-hamt-dictionary))
  (let ((root (access-root container)))
    (make 'transactional-hamt-dictionary
          :hash-fn (cl-ds.dicts:read-hash-fn container)
          :root root
          :equal-fn (cl-ds.dicts:read-equal-fn container)
          :root-was-modified (access-root-was-modified container)
          :size (access-size container))))


(defmethod cl-ds:become-mutable ((container transactional-hamt-dictionary))
  (let ((root (access-root container)))
    (when (and root (hash-node-p root))
      (clear-modification-masks root))
    (make 'mutable-hamt-dictionary
          :hash-fn (cl-ds.dicts:read-hash-fn container)
          :root root
          :equal-fn (cl-ds.dicts:read-equal-fn container)
          :size (access-size container))))


(defmethod cl-ds:become-functional ((container transactional-hamt-dictionary))
  (let ((root (access-root container)))
    (when (and root (hash-node-p root))
      (clear-modification-masks root))
    (make 'functional-hamt-dictionary
          :hash-fn (cl-ds.dicts:read-hash-fn container)
          :root root
          :equal-fn (cl-ds.dicts:read-equal-fn container)
          :size (access-size container))))


(flet ((fn (x)
         (list* (cl-ds.common:hash-content-location x)
                (cl-ds.common:hash-dict-content-value x))))
  (defmethod get-range-key-function ((container hamt-dictionary))
    #'fn))


(defmethod cl-ds:whole-range ((container mutable-hamt-dictionary))
  (make 'cl-ds.common:assignable-forward-tree-range
        :obtain-value #'obtain-value
        :key (get-range-key-function container)
        :forward-stack (list (new-cell (access-root container)))
        :store-value (lambda (node value) (setf (cl-ds.common:hash-dict-content-value node) value))
        :container container))
