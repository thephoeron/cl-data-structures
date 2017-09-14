(in-package #:cl-ds.dicts.hamt)


(defclass functional-hamt-dictionary (hamt-dictionary
                                      cl-ds:functional)
  ())


(defclass mutable-hamt-dictionary (hamt-dictionary
                                   cl-ds:mutable)
  ())


(defclass transactional-hamt-dictionary (cl-ds:transactional hamt-dictionary)
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


(-> functional-hamt-dictionary-erase (functional-hamt-dictionary t)
    (values functional-hamt-dictionary
            cl-ds:fundamental-modification-operation-status))
(defun functional-hamt-dictionary-erase (container location)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "Implementation of ERASE"
  (with-hash-tree-functions (container)
    (let ((hash (hash-fn location)))
      (multiple-value-bind (new-root found old-value)
          (copying-erase-implementation container
                                        hash
                                        location
                                        #'copy-on-write
                                        nil)
        (if found
            (values (make (type-of container)
                          :hash-fn (read-hash-fn container)
                          :root new-root
                          :equal-fn (read-equal-fn container)
                          :max-depth (read-max-depth container)
                          :size (1- (access-size container)))
                    (cl-ds.common:make-eager-modification-operation-status
                     t old-value))
            (values container
                    cl-ds.common:empty-eager-modification-operation-status))))))


(-> transactional-hamt-dictionary-erase! (transactional-hamt-dictionary t)
    (values transactional-hamt-dictionary
            cl-ds:fundamental-modification-operation-status))
(defun transactional-hamt-dictionary-erase! (container location)
  (declare (optimize (speed 3)))
  "Implementation of ERASE!"
  (with-hash-tree-functions (container)
    (let ((hash (hash-fn location)))
      (multiple-value-bind (new-root found old-value)
          (copying-erase-implementation container
                                        hash
                                        location
                                        #'transactional-copy-on-write
                                        (list (access-root-was-modified
                                               container)))
        (when found
          (decf (access-size container)))
        (unless (eq new-root (access-root container))
          (setf (access-root-was-modified container) t
                (access-root container) new-root))
        (values container
                (cl-ds.common:make-eager-modification-operation-status
                 found
                 old-value))))))


(-> mutable-hamt-dictionary-insert! (mutable-hamt-dictionary t t)
    (values t
            cl-ds:fundamental-modification-operation-status))
(defun mutable-hamt-dictionary-insert! (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)
                     (space 0) (compilation-speed 0)))
  "Implementation of (SETF AT)"
  (with-hash-tree-functions (container)
    (let ((replaced nil)
          (old-value nil)
          (hash (hash-fn location)))
      (flet ((destructive-insert (node)
               (multiple-value-bind (next-list r v)
                   (insert-or-replace
                    (access-conflict (the conflict-node node))
                    (make-hash.location.value :hash hash
                                              :location location
                                              :value new-value)
                    :test #'compare-fn)
                 (setf (access-conflict node) next-list
                       replaced r
                       old-value (hash.location.value-value v))
                 node)))
        (declare (dynamic-extent (function destructive-insert))
                 (inline destructive-insert))
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
                                     (make-conflict-node
                                      (list (make-hash.location.value
                                             :hash hash
                                             :location location
                                             :value new-value))))
                                    prev-index)
                                   root)
                                 (make-conflict-node
                                  (list (make-hash.location.value
                                         :hash hash
                                         :location location
                                         :value new-value))))
                     :on-leaf (if prev-node
                                  (progn
                                    (assert (hash-node-contains-leaf
                                             prev-node prev-index))
                                    (hash-node-replace!
                                     prev-node
                                     (rebuild-rehashed-node
                                      c
                                      (read-max-depth container)
                                      (destructive-insert node))
                                     prev-index)
                                    root)
                                  (rebuild-rehashed-node
                                   c
                                   (read-max-depth container)
                                   (destructive-insert node))))))
          (setf (access-root container) result)
          (unless replaced
            (incf (the fixnum (access-size container))))
          (values new-value
                  (cl-ds.common:make-eager-modification-operation-status
                   replaced
                   old-value)))))))


(-> functional-hamt-dictionary-update (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary
            cl-ds:fundamental-modification-operation-status))
(defun functional-hamt-dictionary-update (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of UPDATE"
  (with-hash-tree-functions (container)
    (let ((hash (hash-fn location)))
      (multiple-value-bind (new-root found old-value)
          (copying-update-implementation container hash location new-value
                                         #'copy-on-write
                                         nil)
        (if found
            (values (make (type-of container)
                          :equal-fn (read-equal-fn container)
                          :hash-fn (read-hash-fn container)
                          :root new-root
                          :max-depth (read-max-depth container)
                          :size (access-size container))
                    (cl-ds.common:make-eager-modification-operation-status
                     found
                     old-value))
            (values container
                    cl-ds.common:empty-eager-modification-operation-status))))))


(-> functional-hamt-dictionary-add (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary
            cl-ds:fundamental-modification-operation-status))
(defun functional-hamt-dictionary-add (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of ADD"
  (with-hash-tree-functions (container)
    (let* ((hash (hash-fn location)))
      (multiple-value-bind (new-root found old)
          (copying-add-implementation container hash location new-value
                                      #'copy-on-write nil)
        (if found
            (values container
                    (cl-ds.common:make-eager-modification-operation-status
                     t old))
            (values (make (type-of container)
                          :equal-fn (read-equal-fn container)
                          :hash-fn (read-hash-fn container)
                          :root new-root
                          :max-depth (read-max-depth container)
                          :size (1+ (access-size container)))
                    cl-ds.common:empty-eager-modification-operation-status))))))


(-> mutable-hamt-dictionary-update! (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary
            cl-ds:fundamental-modification-operation-status))
(defun mutable-hamt-dictionary-update! (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of UPDATE!"
  (with-hash-tree-functions (container)
    (let ((hash (hash-fn location)))
      (flet ((location-test (loc location)
               (let ((result
                       (and (eql
                             hash (hash.location.value-hash location))
                            (equal-fn
                             loc (hash.location.value-location location)))))
                 result)))
        (hash-do
            (node index)
            ((access-root container) hash)
            :on-leaf (if-let ((r (find
                                  location
                                  (the list (access-conflict
                                             (the conflict-node node)))
                                  :test #'location-test)))
                       (let ((old (hash.location.value-value r)))
                         (setf (hash.location.value-value r) new-value
                               (hash.location.value-hash r) hash)
                         (values
                          container
                          (cl-ds.common:make-eager-modification-operation-status
                           t old)))
                       (values
                        container
                        cl-ds.common:empty-eager-modification-operation-status))
            :on-nil (values
                     container
                     cl-ds.common:empty-eager-modification-operation-status))))))


(-> mutable-hamt-dictionary-add! (mutable-hamt-dictionary t t)
    (values mutable-hamt-dictionary
            cl-ds:fundamental-modification-operation-status))
(defun mutable-hamt-dictionary-add! (container location new-value)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  "Implementation of add!"
  (with-hash-tree-functions (container)
    (let ((hash (hash-fn location)))
      (flet ((destructive-insert (node)
               (multiple-value-bind (next-list r v)
                   (insert-or-replace
                    (access-conflict (the conflict-node node))
                    (make-hash.location.value :hash hash
                                              :location location
                                              :value new-value)
                    :test #'compare-fn)
                 (when r
                   (return-from mutable-hamt-dictionary-add!
                     (values
                      container
                      (cl-ds.common:make-eager-modification-operation-status
                       t
                       (hash.location.value-value v)))))
                 (setf (access-conflict node) next-list)
                 node)))
        (let* ((prev-node nil)
               (prev-index 0)
               (root (access-root container))
               (result
                 (hash-do
                     (node index c)
                     ((access-root container) hash)
                     :on-every (setf prev-node node
                                     prev-index index)
                     :on-nil (if prev-node
                                 (progn
                                   (assert (not (hash-node-contains-leaf
                                                 prev-node prev-index)))
                                   (hash-node-insert!
                                    prev-node
                                    (rebuild-rehashed-node
                                     c
                                     (read-max-depth container)
                                     (make-conflict-node
                                      (list (make-hash.location.value
                                             :hash hash
                                             :location location
                                             :value new-value))))
                                    prev-index)
                                   root)
                                 (make-conflict-node
                                  (list (make-hash.location.value
                                         :hash hash
                                         :location location
                                         :value new-value))))
                     :on-leaf (if prev-node
                                  (progn
                                    (assert (hash-node-contains-leaf
                                             prev-node prev-index))
                                    (hash-node-replace!
                                     prev-node
                                     (rebuild-rehashed-node
                                      c
                                      (read-max-depth container)
                                      (destructive-insert node))
                                     prev-index)
                                    root)
                                  (rebuild-rehashed-node
                                   c
                                   (read-max-depth container)
                                   (destructive-insert node))))))
          (setf (access-root container) result)
          (incf (access-size container))
          (values container
                  cl-ds.common:empty-eager-modification-operation-status))))))


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


(-> transactional-hamt-dictionary-insert! (transactional-hamt-dictionary t t)
    (values t
            cl-ds:fundamental-modification-operation-status))
(defun transactional-hamt-dictionary-insert! (container location new-value)
  (declare (optimize (speed 3)))
  "Implementation of (setf (at container location) new-value)"
  (with-hash-tree-functions (container)
    (let ((hash (hash-fn location)))
      (multiple-value-bind (new-root found old-value)
          (copying-insert-implementation
           container hash location new-value
           #'transactional-copy-on-write
           (list
            (access-root-was-modified container)))
        (unless (eq (access-root container) new-root)
          (setf (access-root-was-modified container) t
                (access-root container) new-root))
        (unless found
          (incf (the fixnum (access-size container))))
        (values
         new-value
         (cl-ds.common:make-eager-modification-operation-status
          found
          old-value))))))


(-> transactional-hamt-dictionary-update! (transactional-hamt-dictionary t t)
    (values transactional-hamt-dictionary
            cl-ds:fundamental-modification-operation-status))
(defun transactional-hamt-dictionary-update! (container location new-value)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (compilation-speed 0)))
  "Implementation of UPDATE!"
  (with-hash-tree-functions (container)
    (let ((hash (hash-fn location)))
      (multiple-value-bind (new-root found old-value)
          (copying-update-implementation
           container hash location new-value
           #'transactional-copy-on-write
           (list (access-root-was-modified container)))
        (unless (eq new-root (access-root container))
          (setf (access-root container) new-root
                (access-root-was-modified container) new-root))
        (values
         container
         (cl-ds.common:make-eager-modification-operation-status
          found
          old-value))))))


(-> transactional-hamt-dictionary-add! (transactional-hamt-dictionary t t)
    (values transactional-hamt-dictionary
            cl-ds:fundamental-modification-operation-status))
(defun transactional-hamt-dictionary-add! (container location new-value)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (compilation-speed 0)))
  "Implementation of ADD!"
  (with-hash-tree-functions (container)
    (let* ((hash (hash-fn location)))
      (multiple-value-bind (new-root found old)
          (copying-add-implementation
           container hash location new-value
           #'transactional-copy-on-write
           (list (access-root-was-modified container)))
        (unless found
          (incf (access-size container)))
        (unless (eq new-root (access-root container))
          (setf (access-root container) new-root
                (access-root-was-modified container) t))
        (if found
            (values
             container
             (cl-ds.common:make-eager-modification-operation-status
              found old)))))))


#|

Methods. Those will just call non generic functions.

|#


(defmethod cl-ds:update! ((container mutable-hamt-dictionary) location new-value)
  (mutable-hamt-dictionary-update! container location new-value))


(defmethod (setf cl-ds:at) (new-value (container mutable-hamt-dictionary) location)
  (mutable-hamt-dictionary-insert! container location new-value))


(defmethod cl-ds:erase! ((container mutable-hamt-dictionary) location)
  (mutable-hamt-dictionary-erase! container location))


(defmethod cl-ds:add! ((container mutable-hamt-dictionary) location new-value)
  (mutable-hamt-dictionary-add! container location new-value))


(defmethod cl-ds:size ((container hamt-dictionary))
  (hamt-dictionary-size container))


(defmethod cl-ds:at ((container hamt-dictionary) location)
  (hamt-dictionary-at container location))


(defmethod cl-ds:position-modification ((operation cl-ds:grow-function)
                                        (container functional-hamt-dictionary)
                                        location &key value)
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
                            :size (1+ (access-size container)))
                      container)
                  status))))))


(defmethod cl-ds:erase ((container functional-hamt-dictionary) location)
  (functional-hamt-dictionary-erase container location))


(defmethod (setf cl-ds:at) (new-value (container transactional-hamt-dictionary) location)
  (transactional-hamt-dictionary-insert! container location new-value))


(defmethod cl-ds:erase! ((container transactional-hamt-dictionary) location)
  (transactional-hamt-dictionary-erase! container location))


(defmethod cl-ds:add! ((container transactional-hamt-dictionary) location new-value)
  (transactional-hamt-dictionary-add! container location new-value))


(defmethod cl-ds:update! ((container transactional-hamt-dictionary) location new-value)
  (transactional-hamt-dictionary-update! container location new-value))


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
