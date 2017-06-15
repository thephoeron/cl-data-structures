(in-package #:cl-ds.dicts.hamt)

#|

Basic types

|#

(define-constant +hash-level+ 6)


(deftype maybe-node ()
  `(or null hash-node bottom-node))


(deftype node-position ()
  `(values maybe-node fixnum))


(deftype hash-node-index ()
  `(integer 0 ,(ash 2 (1- +hash-level+))))


(deftype hash-mask ()
  `(unsigned-byte ,(ash 2 (1- +hash-level+))))


(deftype just-node ()
  `(or hash-node bottom-node))

#|

Macros

|#

(defmacro hash-do ((node index &optional (count (gensym)))
                   (root hash &optional (max-depth 10))
                   &key on-leaf on-nil on-every)
  "Macro used for writing code going down into hash tree."
  (with-gensyms (!pos !block !leaf)
    (once-only (hash root max-depth)
      `(block ,!block
         (assert (<= ,max-depth 10))
         (do ((,!pos ,+hash-level+ (+ ,!pos ,+hash-level+))
              (,index (ldb (byte ,+hash-level+ 0) ,hash)
                      (ldb (byte ,+hash-level+ ,!pos) ,hash))
              (,count 0 (1+ ,count))
              (,!leaf (and ,root (not (hash-node-p ,root)))
                      (hash-node-contains-leaf ,node ,index))
              (,node ,root (and (hash-node-contains ,node ,index)
                                (hash-node-access ,node ,index))))
             ((= ,count ,max-depth)
              (values ,node
                      ,count))
           (declare (type fixnum ,hash ,!pos ,index ,count))
           (progn
             ,(when on-nil
                `(unless ,node
                   (return-from ,!block
                     ,on-nil)))
             ,(when on-leaf
                `(let ((,node ,node))
                   (declare (type bottom-node ,node))
                   (when ,!leaf
                      (return-from ,!block
                        ,on-leaf))))
             ,on-every
             (let ((,node ,node))
               (declare (type hash-node ,node))
               (when (or ,!leaf (null ,node))
                 (return-from ,!block ,node)))))))))


(defmacro with-hash-tree-functions (container &body body)
  "Simple macro adding local functions (all forwards to the container closures)."
  (once-only (container)
    `(fbind ((equal-fn (read-equal-fn ,container))
             (hash-fn (read-hash-fn ,container)))
       (declare (ignorable (function hash-fn)
                           (function equal-fn)))
       (flet ((compare-fn (a b)
                (the boolean (same-location a b (read-equal-fn ,container)))))
         (declare (ignorable (function compare-fn)))
         ,@body))))


(defmacro with-hamt-path (node container hash &key on-leaf on-nil operation)
  (with-gensyms (!count !path !indexes !depth !max-depth !root !index)
    (once-only (container)
      `(let* ((,!max-depth (read-max-depth ,container))
              (,!path (make-array 12))
              (,!indexes (make-array 12 :element-type 'fixnum))
              (,!depth 0)
              (,!root (access-root ,container)))
         (declare (type fixnum ,!depth)
                  (type (simple-array fixnum (12)) ,!indexes)
                  (type (simple-vector 12) ,!path)
                  (dynamic-extent ,!path ,!indexes ,!depth))
         (hash-do
             (,node ,!index ,!count)
             (,!root ,hash ,!max-depth)
             :on-every (progn (setf (aref ,!path ,!count) ,node
                                    (aref ,!indexes ,!count) ,!index)
                              (incf ,!depth))
             :on-nil (let ((next ,on-nil))
                       (,operation ,!indexes ,!path ,!depth next))
             :on-leaf (let ((next ,on-leaf))
                        (,operation ,!indexes ,!path ,!depth next)))))))


(defmacro with-destructive-erase-hamt (node container hash &key on-leaf on-nil)
  (with-gensyms (!path !depth !indexes !rewrite)
    (once-only (container)
      `(flet ((,!rewrite (,!indexes ,!path ,!depth conflict) ;path and indexes have constant size BUT only part of it is used, that's why length is passed here
                (declare (type (simple-array fixnum) ,!indexes)
                         (type simple-array ,!path)
                         (type fixnum ,!depth)
                         (type maybe-node conflict))
                (with-vectors (,!path ,!indexes)
                  (iterate
                    (for i from (- ,!depth 1) downto 0) ;reverse order (starting from deepest node)
                    (for node = (,!path i))
                    (for index = (,!indexes i))
                    (for ac initially conflict
                         ;;rehash actually returns cl:hash-table, build-rehashed-node transforms it into another hash-node, depth is increased by 1 this way
                         then (if ac
                                  (progn (hash-node-replace! node ac index)
                                         (finish))
                                  (if (eql 1 (hash-node-size node))
                                      ac
                                      (hash-node-remove! node index))))
                    (finally (return ac))))))
         (declare (dynamic-extent (function ,!rewrite))
                  (inline ,!rewrite))
         (with-hamt-path ,node
           ,container ,hash
           :on-leaf ,on-leaf
           :on-nil ,on-nil
           :operation ,!rewrite)))))


(-> set-in-leaf-mask (hash-node hash-node-index (integer 0 1)) hash-node)
(defun set-in-leaf-mask (node position bit)
  (declare (optimize (speed 3) (space 0) (safety 0)))
  (setf (ldb (byte 1 position) (hash-node-leaf-mask node)) bit)
  node)


(-> set-in-node-mask (hash-node hash-node-index (integer 0 1)) hash-node)
(defun set-in-node-mask (node position bit)
  (declare (optimize (speed 3) (space 0) (safety 0)))
  (setf (ldb (byte 1 position) (hash-node-node-mask node)) bit)
  node)


#|

Tree structure of HAMT

|#


(defstruct hash-node
  (leaf-mask 0 :type hash-mask)
  (node-mask 0 :type hash-mask)
  (modification-mask 0 :type hash-mask)
  (content #() :type simple-array))


(declaim (inline make-hash-node))


(defclass bottom-node () ()
  (:documentation "Base class of the last (conflict) node. Subclasses present to dispatch relevant logic."))


(defstruct hash.location.value
  (hash 0 :type fixnum)
  location
  value)


(declaim (inline make-hash.location.value))


(-> same-location (hash.location.value hash.location.value (-> (t t) boolean)) boolean)
(defun same-location (existing new-location equal-fn)
  (declare (optimize (speed 3)))
  (and (eql (hash.location.value-hash existing)
            (hash.location.value-hash new-location))
       (funcall equal-fn
                (hash.location.value-location existing)
                (hash.location.value-location new-location))))


(defclass conflict-node (bottom-node)
  ((%conflict :initarg :conflict
              :accessor access-conflict
              :initform (list)
              :type list
              :documentation "List of elements with conflicting hash."))
  (:documentation "Conflict node simply holds list of elements that are conflicting."))


(-> make-conflict-node (list) conflict-node)
(defun make-conflict-node (content)
  (assure conflict-node (make 'conflict-node :conflict content)))


(defclass box-node (bottom-node)
  ((%content :initarg :content
             :reader read-content
             :documentation "Internal value of box"))
  (:documentation "Box node holds only one element inside."))


(defgeneric empty-node-p (bottom-node))


(defgeneric contains-p (bottom-node item fn))


(defmethod contains-p ((node conflict-node) item fn)
  (find item (access-conflict node) :test fn))


(defmethod empty-node-p ((node box-node))
  (slot-boundp node '%content))


(defmethod empty-node-p ((node conflict-node))
  (endp (access-conflict node)))


#|

Interface class.

|#

(defclass fundamental-hamt-container (cl-ds:fundamental-container)
  ((%root :type (or hash-node bottom-node null)
          :accessor access-root
          :initarg :root
          :documentation "Hash node pointing to root of the whole hash tree.")
   (%hash-fn :type (-> (x) fixnum)
             :reader read-hash-fn
             :initarg :hash-fn
             :documentation "Closure used for key hashing. Setted by the user.")
   (%equal-fn :type (-> (t t) boolean)
              :reader read-equal-fn
              :initarg :equal-fn
              :documentation "Closure used for comparing items at the bottom level lists.")
   (%max-depth :initarg :max-depth
               :type (integer 0 10)
               :reader read-max-depth
               :documentation "Maximal depth of tree.")
   (%size :initarg :size
          :initform 0
          :type non-negative-fixnum
          :accessor access-size
          :documentation "How many elements are in there?"))
  (:documentation "Base class of other containers. Acts as any container for bunch of closures (those vary depending on the concrete container) and root of the tree."))


(defclass hamt-dictionary (fundamental-hamt-container
                           cl-ds.dicts:dictionary)
  ())


#|

Functions with basic bit logic.

|#

(-> hash-node-whole-mask (hash-node) (unsigned-byte 64))
(defun hash-node-whole-mask (node)
  (logior (hash-node-node-mask node) (hash-node-leaf-mask node)))


(declaim (inline hash-node-whole-mask))


(-> hash-node-to-masked-index (hash-node (hash-node-index)) hash-node-index)
(defun hash-node-to-masked-index (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> hash-node
       hash-node-whole-mask
       (ldb (byte index 0))
       logcount))


(declaim (inline hash-node-to-masked-index))


(-> hash-node-contains (hash-node hash-node-index) boolean)
(defun hash-node-contains (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> (hash-node-whole-mask hash-node)
       (ldb (byte 1 index))
       zerop
       not))


(-> hash-node-contains-leaf (hash-node hash-node-index) boolean)
(defun hash-node-contains-leaf (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> (hash-node-leaf-mask hash-node)
       (ldb (byte 1 index))
       zerop
       not))


(-> hash-node-contains-node (hash-node hash-node-index) boolean)
(defun hash-node-contains-node (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> (hash-node-node-mask hash-node)
       (ldb (byte 1 index))
       zerop
       not))


(declaim (inline hash-node-contains))
(declaim (inline hash-node-contains-leaf))
(declaim (inline hash-node-contains-node))


(-> hash-node-access (hash-node hash-node-index) t)
(defun hash-node-access (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (handler-case
      (~>> (hash-node-to-masked-index hash-node index)
           (aref (hash-node-content hash-node)))))


(declaim (inline hash-node-access))


(-> hash-node-size (hash-node) (integer 0 64))
(defun hash-node-size (node)
  (logcount (hash-node-whole-mask node)))

#|

Copy nodes and stuff.

|#


(-> go-down-on-path (fundamental-hamt-container fixnum
                                                function list
                                                function list
                                                function list)
    (values maybe-node boolean t))
(defun go-down-on-path (container hash on-leaf on-leaf-args on-nil  on-nil-args after after-args)
  (declare (optimize (debug 3) (safety 0)))
  (let ((old-value nil)
        (found nil))
    (flet ((after (indexes path depth next)
             (the maybe-node (apply after indexes path depth (read-max-depth container) next after-args))))
      (let ((result (with-hamt-path node container hash
                      :operation after
                      :on-leaf (multiple-value-bind (n f o) (apply on-leaf node on-leaf-args)
                                 (setf old-value o
                                       found f)
                                 n)
                      :on-nil (multiple-value-bind (n f o) (apply on-nil on-nil-args)
                                (setf old-value o
                                      found f)
                                n))))
        (values result found old-value)))))


(-> copy-node (hash-node &key
                         (:leaf-mask (unsigned-byte 64))
                         (:node-mask (unsigned-byte 64))
                         (:modification-mask (unsigned-byte 64))
                         (:content simple-vector))
    hash-node)
(defun copy-node (node &key leaf-mask node-mask content modification-mask)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (make-hash-node
   :leaf-mask (or leaf-mask (hash-node-leaf-mask node))
   :node-mask (or node-mask (hash-node-node-mask node))
   :content (or content (hash-node-content node))
   :modification-mask (or modification-mask (hash-node-modification-mask node))))


(-> hash-node-replace-in-the-copy (hash-node t hash-node-index) hash-node)
(defun hash-node-replace-in-the-copy (hash-node item index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (let* ((content (copy-array (hash-node-content hash-node)))
         (leaf-mask (hash-node-leaf-mask hash-node))
         (node-mask (hash-node-node-mask hash-node)))
    (declare (type (unsigned-byte 64) leaf-mask node-mask))
    (if (hash-node-p item)
        (setf (ldb (byte 1 index) node-mask) 1
              (ldb (byte 1 index) leaf-mask) 0)
        (setf (ldb (byte 1 index) node-mask) 0
              (ldb (byte 1 index) leaf-mask) 1))
    (setf (aref content
                (logcount (ldb (byte index 0) (logior leaf-mask node-mask))))
          item)
    (copy-node hash-node
               :leaf-mask leaf-mask
               :node-mask node-mask
               :content content)))


(declaim (inline hash-node-replace-in-the-copy))


(-> hash-node-insert-into-copy (hash-node t hash-node-index) hash-node)
(defun hash-node-insert-into-copy (hash-node content index)
  (let ((position (hash-node-to-masked-index hash-node index)))
    (with-vectors ((current-array (hash-node-content hash-node))
                   (new-array (make-array (1+ (array-dimension current-array 0)))))
      (assert (~> (array-dimension new-array 0)
                  (<= 64)))
      ;;before new element
      (iterate

        (for i from 0 below position)
        (setf (new-array i)
              (current-array i)))

      ;;new element
      (setf (new-array position)
            content)

      ;;after new element
      (iterate
        (for i from position below (array-dimension current-array 0))
        (setf (new-array (1+ i))
              (current-array i)))

      ;;just make new hash-node
      (let ((node-mask (hash-node-node-mask hash-node))
            (leaf-mask (hash-node-leaf-mask hash-node)))
        (if (hash-node-p content)
            (setf (ldb (byte 1 index) node-mask) 1)
            (setf (ldb (byte 1 index) leaf-mask) 1))
        (copy-node hash-node
                   :node-mask node-mask
                   :leaf-mask leaf-mask
                   :content new-array)))))


(defun non-empty-hash-table-p (table)
  (and (typep table 'hash-table)
       (not (zerop (hash-table-count table)))))


(deftype non-empty-hash-table ()
  `(satisfies non-empty-hash-table-p))


(defgeneric rehash (conflict level cont)
  (:documentation "Attempts to divide conflct into smaller ones. Retudnerd hash table maps position of conflict to conflict itself and should contain at least one element"))


(defgeneric single-elementp (conflict)
  (:documentation "Checks if conflict node holds just a single element. Returns t if it does, returns nil if it does not."))


(-> rebuild-rehashed-node (fixnum fixnum bottom-node) just-node)
(-> build-rehashed-node (fixnum fixnum (simple-vector 64)) just-node)
(defun build-rehashed-node (depth max-depth content)
  (let ((mask 0)
        (node-mask 0)
        (leaf-mask 0)
        (size 0))
    (iterate
      (for elt in-vector content)
      (for index from 0)
      (when elt
        (incf size)
        (setf (ldb (byte 1 index) mask) 1)))
    (with-vectors ((array (make-array size)))
      (iterate
        (for conflict in-vector content)
        (for index from 0)
        (when conflict
          (for i = (logcount (ldb (byte index 0) mask)))
          (setf (array i)
                (rebuild-rehashed-node depth
                                       max-depth
                                       conflict))
          (if (hash-node-p (array i))
              (setf (ldb (byte 1 index) node-mask) 1)
              (setf (ldb (byte 1 index) leaf-mask) 1))))
      (make-hash-node :leaf-mask leaf-mask
                      :node-mask node-mask
                      :content array))))


(let ((max-mask (iterate
                  (for i from 0 below 64)
                  (for result
                       initially 0
                       then (dpb 1 (byte 1 i) result))
                  (finally (return result)))))
  (defun mark-everything-as-modified (node)
    (setf (hash-node-modification-mask node)
          max-mask)
    node))


(defun transactional-rebuild-rehashed-node (depth max-depth conflict)
  (flet ((cont (array)
           (let ((result (build-rehashed-node (1+ depth) max-depth array)))
             (mark-everything-as-modified result))))
    (declare (dynamic-extent #'cont))
    (if (or (>= depth max-depth) (single-elementp conflict))
        conflict
        (rehash conflict depth
                #'cont))))


(defun rebuild-rehashed-node (depth max-depth conflict)
  (flet ((cont (array)
           (build-rehashed-node (1+ depth) max-depth array)))
    (declare (dynamic-extent #'cont))
    (if (or (>= depth max-depth) (single-elementp conflict))
        conflict
        (rehash conflict depth
                #'cont))))


(-> build-node (hash-node-index just-node) hash-node)
(defun build-node (index content)
  (if (hash-node-p content)
      (make-hash-node :node-mask (ash 1 index)
                      :content (make-array 1 :initial-element content))
      (make-hash-node :leaf-mask (ash 1 index)
                      :content (make-array 1 :initial-element content))))


(-> hash-node-insert! (hash-node t hash-node-index) hash-node)
(defun hash-node-insert! (node content index)
  (assert (zerop (ldb (byte 1 index) (hash-node-whole-mask node))))
  (let* ((next-size (~> node
                        hash-node-content
                        (array-dimension 0)
                        1+))
         (next-mask (~>> node
                         hash-node-whole-mask
                         (dpb 1 (byte 1 index))))
         (masked-index (~>> next-mask
                            (ldb (byte index 0))
                            logcount)))
    (with-vectors ((n (make-array next-size)) (s (hash-node-content node)))
      (iterate
        (for i from 0 below next-size)
        (cond-compare (i masked-index)
                      (setf (n i) (s i))
                      (setf (n i) content)
                      (setf (n i) (s (1- i)))))
      (setf (hash-node-content node) n)
      (if (hash-node-p content)
          (set-in-node-mask node index 1)
          (set-in-leaf-mask node index 1))
      node)))


(defun hash-node-replace! (node content index)
  (assert (not (zerop (ldb (byte 1 index) (hash-node-whole-mask node)))))
  (with-vectors ((a (hash-node-content node)))
    (setf (a (hash-node-to-masked-index node index))
          content)
    (if (hash-node-p content)
        (progn (set-in-node-mask node index 1)
               (set-in-leaf-mask node index 0))
        (progn (set-in-node-mask node index 0)
               (set-in-leaf-mask node index 1))))
  node)


(-> hash-node-remove-from-the-copy (hash-node fixnum) hash-node)
(-> hash-node-remove! (hash-node fixnum) hash-node)
(flet ((new-array (node index)
         (copy-without (hash-node-content node)
                       (1- (logcount (ldb (byte (1+ index) 0)
                                          (hash-node-whole-mask node)))))))

  (defun hash-node-remove-from-the-copy (node index)
    "Returns copy of node, but without element under index. Not safe, does not check if element is actually present."
    (copy-node node
               :leaf-mask (dpb 0 (byte 1 index) (hash-node-leaf-mask node))
               :node-mask (dpb 0 (byte 1 index) (hash-node-node-mask node))
               :content (new-array node index)))

  (defun hash-node-remove! (node index)
    (setf (hash-node-content node)
          (new-array node index))
    (set-in-leaf-mask node index 0)
    (set-in-node-mask node index 0)
    node))


(-> map-hash-tree ((-> (bottom-node) t) hash-node) hash-node)
(defun map-hash-tree (fn root)
  (iterate
    (with stack = (make-array 32
                              :element-type 'maybe-node
                              :adjustable t
                              :fill-pointer 1
                              :initial-element root))
    (for current = (pop-last stack))
    (while current)
    (for (node . hash-path) = current)
    (etypecase node
      (bottom-node (funcall fn node))
      (hash-node (with-accessors ((mask hash-node-whole-mask)
                                  (content hash-node-content)) node
                   (iterate
                     (for i from 0 below 64)
                     (with index = 0)
                     (unless (~> (ldb (byte 1 i) mask)
                                 zerop)
                       (vector-push-extend (aref content index)
                                           stack)
                       (incf index)))))
      (t (assert (null node)))))
  root)


(-> contains-part-of-hash (fixnum fixnum (integer 0 64)) boolean)
(defun contains-part-of-hash (hash partial-hash depth)
  (~>> hash
       (logxor partial-hash)
       (ldb (byte depth 0))
       zerop))


(defmethod rehash (conflict level cont)
  (declare (type conflict-node conflict))
  (let ((result (make-array 64 :initial-element nil))
        (byte (byte +hash-level+ (* +hash-level+ level))))
    (declare (dynamic-extent byte)
             (dynamic-extent result))
    (iterate
      (for item in (access-conflict conflict))
      (for hash = (hash.location.value-hash item))
      (for index = (ldb byte hash))
      (push item (access-conflict (ensure (aref result index)
                                    (make 'conflict-node)))))
    (funcall cont result)))


(defmethod single-elementp ((conflict conflict-node))
  (endp (cdr (access-conflict conflict))))


(defgeneric print-hamt (obj stream &optional indent)
  (:method ((obj hash-node) stream &optional indent)
    (ensure indent 0)
    (format stream "~v@{~a~:*~}<HN: " indent " ")
    (format stream "~b~%" (hash-node-whole-mask obj))
    (iterate
      (for elt in-vector (hash-node-content obj))
      (for i from 1)
      (print-hamt elt stream (1+ indent))
      (unless (eql i (length (hash-node-content obj)))
        (format stream "~%")))
    (format stream ">")
    obj)
  (:method ((obj (eql nil)) stream &optional indent)
    (ensure indent 0)
    (format stream "~v@{~a~:*~}EMPTY" indent " ")
    obj)
  (:method ((obj conflict-node) stream &optional indent)
    (ensure indent 0)
    (format stream "~v@{~a~:*~}(" indent " ")
    (iterate
      (for sub on (access-conflict obj))
      (for elt = (first sub))
      (for key = (hash.location.value-location elt))
      (for value = (hash.location.value-value elt))
      (if (cdr sub)
          (format stream "~A:~A, " key value)
          (format stream "~A:~A" key value)))
    (format stream ")")
    obj))


(defmethod print-object ((obj hash-node) stream)
  (print-hamt obj stream)
  obj)


(defmethod print-object ((obj conflict-node) stream)
  (print-hamt obj stream)
  obj)


(-> copy-on-write ((vector fixnum) vector fixnum fixnum maybe-node) maybe-node)
(defun copy-on-write (indexes path depth max-depth conflict)
  (declare (type (simple-array fixnum) indexes)
           (type simple-array path)
           (type fixnum depth)
           (type maybe-node conflict)
           (optimize (speed 3)))
  (with-vectors (path indexes)
    (when (and (not (zerop depth)) (eq conflict (path (- depth 1))))
      (return-from copy-on-write (path 0)))
    (iterate
      (for i from (- depth 1) downto 0) ;reverse order (starting from deepest node)
      (for node = (path i))
      (for index = (indexes i))
      (for ac initially (if (or (hash-node-p conflict)
                                (null conflict))
                            ;;if we didn't find element or element was found but depth was already maximal,
                            ;;we will just return element, otherwise attempt to divide (rehash) conflicting node into few more
                            conflict
                            (rebuild-rehashed-node depth
                                                   max-depth
                                                   conflict))
           then (if ac
                    (if (hash-node-contains node index)
                        (hash-node-replace-in-the-copy node ac index)
                        (hash-node-insert-into-copy node ac index))
                    (if (eql 1 (hash-node-size node))
                        ac
                        (hash-node-remove-from-the-copy node index))))
      (finally (return ac)))))


(-> hash-node-content-modified (hash-node hash-node-index) boolean)
(defun hash-node-content-modified (node index)
  (~>> node
      hash-node-modification-mask
      (ldb (byte 1 index))
      zerop
      not))


(-> set-modified (hash-node hash-node-index) hash-node)
(defun set-modified (node index)
  (setf (ldb (byte 1 index) (hash-node-modification-mask node)) 1)
  node)


(-> hash-node-transactional-replace (boolean hash-node just-node hash-node-index) hash-node)
(defun hash-node-transactional-replace (must-copy node content index)
  (let ((result (if must-copy
                    (hash-node-replace-in-the-copy node content index)
                    (hash-node-replace! node content index))))
    (set-modified result index)
    result))


(-> hash-node-transactional-insert (boolean hash-node just-node hash-node-index) hash-node)
(defun hash-node-transactional-insert (must-copy node content index)
  (let ((result (if must-copy
                    (hash-node-insert-into-copy node content index)
                    (hash-node-insert! node content index))))
    (set-modified result index)
    result))


(-> hash-node-transactional-remove (boolean hash-node hash-node-index) hash-node)
(defun hash-node-transactional-remove (must-copy node index)
  (let ((result (if must-copy
                    (hash-node-remove! node index)
                    (hash-node-remove-from-the-copy node index))))
    (set-modified result index)
    result))


(-> transactional-copy-on-write ((vector fixnum) vector fixnum fixnum maybe-node boolean) maybe-node)
(defun transactional-copy-on-write (indexes path depth max-depth conflict root-already-copied)
  (declare (type (simple-array fixnum) indexes)
           (type simple-array path)
           (type fixnum depth)
           (type maybe-node conflict))
  (with-vectors (path indexes)
    (iterate
      (for i from (- depth 1) downto 0) ;reverse order (starting from deepest node)
      (for node = (path i))
      (for index = (indexes i))
      (for parent = (and (not (zerop i))
                         (path (1- i))))
      (for must-copy = (if parent
                           (hash-node-content-modified parent
                                                       (indexes (1- i)))
                           (not root-already-copied)))
      (for ac initially (if (or (hash-node-p conflict)
                                (null conflict))
                            ;;if we didn't find element or element was found but depth was already maximal,
                            ;;we will just return element, otherwise attempt to divide (rehash) conflicting node into few more
                            conflict
                            (transactional-rebuild-rehashed-node depth
                                                                 max-depth
                                                                 conflict))
           then (if ac
                    (if (hash-node-contains node index)
                        (hash-node-transactional-replace must-copy node ac index)
                        (hash-node-transactional-insert must-copy
                                                        node ac index))
                    (if (eql 1 (hash-node-size node))
                        ac
                        (hash-node-transactional-remove must-copy node index))))
      (when (eq node ac)
        (leave (path 0)))
      (finally (return ac)))))


(-> clear-modification-masks (hash-node) hash-node)
(defun clear-modification-masks (node)
  (iterate
    (for i from 0 below 64)
    (when (and (hash-node-content-modified node i)
               (hash-node-contains-node node i))
      (clear-modification-masks (hash-node-access node i))))
  (setf (hash-node-modification-mask node) 0)
  node)


(defun wrap-conflict (hash location new-value)
  (values (make-conflict-node
           (list
            (make-hash.location.value :hash hash
                                      :location location
                                      :value new-value)))
          nil
          nil))


(defun insert-conflict (node hash location new-value compare-fn)
  (multiple-value-bind
        (next-list replaced old-value)
      (insert-or-replace
       (access-conflict
        (the conflict-node node))
       (make-hash.location.value :hash
                                 hash
                                 :location
                                 location
                                 :value
                                 new-value)
       :test compare-fn)
    (values
     (make-conflict-node next-list)
     replaced
     (and replaced
          (hash.location.value-value
           old-value)))))


(-> copying-insert-implementation
    (fundamental-hamt-container fixnum t t function list)
    (values maybe-node boolean t))
(defun copying-insert-implementation (container hash location new-value after after-args)
  (declare (optimize (speed 3)))
  (with-hash-tree-functions container
    (go-down-on-path container hash
                     #'insert-conflict (list hash location new-value
                                             #'compare-fn)
                     #'wrap-conflict (list hash location new-value)
                     after after-args)))


(-> copying-erase-implementation
    (fundamental-hamt-container fixnum t function list)
    (values maybe-node boolean t))
(defun copying-erase-implementation (container hash location after after-args)
  (declare (optimize (speed 3) (safety 0)))
  (flet ((remove-from-conflict (node)
           (let ((equal-fn (read-equal-fn container)))
             (flet ((location-test (node location)
                      (and (eql hash (hash.location.value-hash node))
                           (funcall equal-fn location
                                    (hash.location.value-location node)))))
               (multiple-value-bind (list removed value)
                   (try-remove location
                               (access-conflict node)
                               :test #'location-test)
                 (unless removed
                   (return-from copying-erase-implementation
                     (values (access-root container) nil nil)))
                 (values (and list (make-conflict-node list))
                         removed
                         (hash.location.value-value value))))))
         (return-nil () (return-from copying-erase-implementation
                          (values (access-root container) nil nil))))
    (go-down-on-path container hash
                     #'remove-from-conflict nil
                     #'return-nil nil
                     after after-args)))


(-> copying-udpate-implementation
    (fundamental-hamt-container fixnum t t function list)
    (values maybe-node boolean t))
(defun copying-update-implementation (container hash location new-value after after-args)
  (declare (optimize (safety 0) (speed 3)))
  (with-hash-tree-functions container
    (flet ((update-in-conflict (node)
             (multiple-value-bind (next-list replaced old-value)
                 (insert-or-replace (access-conflict (the conflict-node node))
                                    (make-hash.location.value :hash hash
                                                              :location location
                                                              :value new-value)
                                    :test #'compare-fn)
               (unless replaced
                 (return-from copying-update-implementation
                   (values (access-root container) nil nil)))
               (values (make-conflict-node next-list)
                       replaced
                       (and replaced old-value
                            (hash.location.value-value old-value)))))
           (return-nil () (return-from copying-update-implementation
                            (values (access-root container) nil nil))))
      (go-down-on-path container hash
                       #'update-in-conflict nil
                       #'return-nil nil
                       after after-args))))


(-> copying-add-implementation
    (fundamental-hamt-container fixnum t t function list)
    (values maybe-node boolean t))
(defun copying-add-implementation (container hash location new-value after after-args)
  (declare (optimize (debug 3) (safety 0)))
  (with-hash-tree-functions container
    (labels ((location-test (location node)
               (and (eql hash (hash.location.value-hash node))
                    (equal-fn location (hash.location.value-location node))))
             (add-into-conflict (node)
               (let* ((list (access-conflict node))
                      (item (find location (the list list)
                                  :test #'location-test)))
                 (when item
                   (return-from copying-add-implementation
                     (values (access-root container)
                             t
                             (hash.location.value-value item))))
                 (values
                  (make-conflict-node (cons (make-hash.location.value
                                             :hash hash
                                             :location location
                                             :value new-value)
                                            list))
                  nil
                  nil))))
      (go-down-on-path container hash
                       #'add-into-conflict nil
                       #'wrap-conflict (list hash location new-value)
                       after after-args))))


(-> hash-node-deep-copy (hash-node) hash-node)
(defun hash-node-deep-copy (node)
  (copy-node node
             :content (copy-array (hash-node-content node))))


(-> isolate-transactional-instance (hash-node boolean) hash-node)
(defun isolate-transactional-instance (parent parent-was-modified)
  (let ((parent (if parent-was-modified
                    (hash-node-deep-copy parent)
                    parent)))
    (with-vectors ((content (hash-node-content parent)))
      (iterate
        (for i from 0 below 64)
        (for was-modified = (and (hash-node-contains-node parent i)
                                 (hash-node-content-modified parent i)))
        (when was-modified
          (let ((masked-index (hash-node-to-masked-index parent i)))
            (setf (content masked-index)
                  (isolate-transactional-instance (content masked-index)
                                                  t))))))
    parent))
