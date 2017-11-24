(in-package #:cl-data-structures.common.hamt)

#|

Basic types

|#

(define-constant +hash-level+ 5)
(define-constant +maximum-children-count+ (ash 1 +hash-level+))
(define-constant +depth+ (floor (/ 64 +hash-level+)))


(deftype hash-node-index ()
  `(integer 0 ,+maximum-children-count+))


(deftype hash-node-size ()
  `(integer 0 ,(1+ +maximum-children-count+)))


(deftype hash-mask ()
  `(unsigned-byte ,+maximum-children-count+))


(deftype node-path ()
  `(simple-vector ,+depth+))


(deftype index-path ()
  `(simple-array fixnum (,+depth+)))

#|

Macros

|#

(defmacro hash-do ((node index &optional (count (gensym)))
                   (root hash)
                   &key on-leaf on-nil on-every)
  "Macro used for writing code going down into hash tree."
  (with-gensyms (!pos !block !leaf)
    (once-only (hash root)
      `(block ,!block
         (do ((,!pos ,+hash-level+ (the fixnum (+ ,!pos ,+hash-level+)))
              (,index (ldb (byte ,+hash-level+ 0) ,hash)
                      (ldb (byte ,+hash-level+ ,!pos) ,hash))
              (,count 0 (1+ ,count))
              (,node ,root (and (hash-node-contains ,node ,index)
                                (hash-node-access ,node ,index))))
             (nil nil)
           (declare (type fixnum ,hash ,!pos ,index ,count)
                    (dynamic-extent ,!pos ,index ,count))
           (let ((,!leaf (and ,node (not (hash-node-p ,node)))))
             ,(when on-nil
                `(when (null ,node)
                   (return-from ,!block
                     ,on-nil)))
             ,(when on-leaf
                `(when ,!leaf
                   (return-from ,!block
                     ,on-leaf)))
             (when (or ,!leaf (null ,node))
               (return-from ,!block ,node))
             ,on-every
             (when (eql ,count ,+depth+)
               (values ,node ,count))))))))


(defmacro with-hamt-path (node hash root &key on-leaf on-nil operation)
  (with-gensyms (!count !path !indexes !depth !root !index)
    `(let* ((,!path (make-array ,+depth+))
            (,!indexes (make-array ,+depth+ :element-type 'fixnum))
            (,!root ,root))
       (declare (type fixnum ,!depth)
                (type index-path ,!indexes)
                (type node-path ,!path)
                (dynamic-extent ,!path ,!indexes))
       (hash-do
           (,node ,!index ,!count)
           (,!root ,hash)
           :on-every (setf (aref ,!path ,!count) ,node
                           (aref ,!indexes ,!count) ,!index)
           :on-nil (let ((next ,on-nil))
                     (,operation ,!indexes ,!path ,!count next))
           :on-leaf (let ((next ,on-leaf))
                      (,operation ,!indexes ,!path ,!count next))))))


(defmacro with-destructive-erase-hamt (node container hash &key on-leaf on-nil)
  (with-gensyms (!path !depth !indexes !rewrite)
    (once-only (container)
      `(flet ((,!rewrite (,!indexes ,!path ,!depth conflict) ;path and indexes have constant size BUT only part of it is used, that's why length is passed here
                (declare (type index-path ,!indexes)
                         (type node-path ,!path)
                         (type fixnum ,!depth))
                (cl-ds.utils:with-vectors (,!path ,!indexes)
                  (iterate
                    (for i from (1- ,!depth) downto 0) ;reverse order (starting from deepest node)
                    (for ,node = (,!path i))
                    (for index = (,!indexes i))
                    (for ac
                         initially conflict
                         then (if ac
                                  (progn (hash-node-replace! ,node ac index)
                                         (leave (aref ,!path 0)))
                                  (if (eql 1 (hash-node-size ,node))
                                      ac
                                      (hash-node-remove! ,node index))))
                    (finally (return ac))))))
         (declare (dynamic-extent (function ,!rewrite))
                  (inline ,!rewrite))
         (with-hamt-path ,node
           ,hash
           (access-root ,container)
           :on-leaf ,on-leaf
           :on-nil ,on-nil
           :operation ,!rewrite)))))


(-> set-in-node-mask (hash-node hash-node-index (integer 0 1)) hash-node)
(defun set-in-node-mask (node position bit)
  (declare (optimize (speed 3) (space 0) (safety 0)))
  (setf (ldb (byte 1 position) (hash-node-node-mask node)) bit)
  node)


#|

Tree structure of HAMT

|#


(defstruct hash-node
  (node-mask 0 :type hash-mask)
  (content #() :type simple-array))


(defstruct (transactional-hash-node (:include hash-node))
  (modification-mask 0 :type hash-mask))


(declaim (inline make-hash-node))


#|

Interface class.

|#


(defclass hamt-container ()
  ((%root :accessor access-root
          :initarg :root
          :documentation "Hash node pointing to root of the whole hash tree.")
   (%size :initarg :size
          :initform 0
          :type non-negative-fixnum
          :accessor access-size
          :documentation "How many elements are in there?")))


#|

Functions with basic bit logic.

|#

(-> hash-node-whole-mask (hash-node) hash-mask)
(defun hash-node-whole-mask (node)
  (hash-node-node-mask node))


(declaim (inline hash-node-whole-mask))


(-> hash-node-to-masked-index (hash-node (hash-node-index)) hash-node-index)
(defun hash-node-to-masked-index (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (~>> hash-node
       hash-node-whole-mask
       (ldb (byte index 0))
       logcount))


(declaim (inline hash-node-to-masked-index))


(-> hash-node-contains (hash-node hash-node-index) boolean)
(defun hash-node-contains (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (~>> (hash-node-whole-mask hash-node)
       (ldb-test (byte 1 index))))


(-> hash-node-contains-node (hash-node hash-node-index) boolean)
(defun hash-node-contains-node (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (~>> (hash-node-node-mask hash-node)
       (ldb-test (byte 1 index))))


(declaim (inline hash-node-contains))
(declaim (inline hash-node-contains-leaf))
(declaim (inline hash-node-contains-node))


(-> hash-node-access (hash-node hash-node-index) t)
(defun hash-node-access (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (~>> (hash-node-to-masked-index hash-node index)
       (aref (hash-node-content hash-node))))


(declaim (inline hash-node-access))
(declaim (inline hash-node-size))


(-> hash-node-size (hash-node) hash-node-size)
(defun hash-node-size (node)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (logcount (hash-node-whole-mask node)))

#|

Copy nodes and stuff.

|#


(-> go-down-on-path (hamt-container fixnum function function function)
    (values t cl-ds.common:eager-modification-operation-status))
(defun go-down-on-path
    (container hash on-leaf on-nil after)
  (declare (optimize (speed 0)
                     (debug 3)
                     (safety 3)
                     (space 0))
           (type fixnum hash))
  (let ((status nil))
    (flet ((after (indexes path depth next)
             (funcall after
                      indexes
                      path
                      depth
                      next)))
      (values (block loop-block
                (with-hamt-path node hash
                  (access-root container)
                  :operation after
                  :on-leaf (multiple-value-bind (b s c)
                               (funcall on-leaf node)
                             (setf status s)
                             (unless c
                               (return-from loop-block nil))
                             b)
                  :on-nil (multiple-value-bind (b s c)
                              (funcall on-nil)
                            (setf status s)
                            (unless c
                              (return-from loop-block nil))
                            b)))
              status))))


(-> copy-node (hash-node &key
                         (:node-mask hash-mask)
                         (:modification-mask hash-mask)
                         (:content simple-vector))
    hash-node)
(declaim (inline copy-node))
(defun copy-node (node &key node-mask content modification-mask)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (if (transactional-hash-node-p node)
      (make-hash-node
       :node-mask (or node-mask (hash-node-node-mask node))
       :content (or content (hash-node-content node)))
      (make-transactional-hash-node
       :node-mask (or node-mask (transactional-hash-node-node-mask node))
       :content (or content (transactional-hash-node-content node))
       :modification-mask (or modification-mask (transactional-hash-node-modification-mask node)))))


(-> hash-node-replace-in-the-copy (hash-node t hash-node-index &key (:transactional boolean)) hash-node)
(declaim (inline hash-node-replace-in-the-copy))
(defun hash-node-replace-in-the-copy (hash-node item index &key (transactional nil))
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 0)
                     (space 0)))
  (let* ((content (copy-array (hash-node-content hash-node)))
         (node-mask (hash-node-node-mask hash-node)))
    (declare (type hash-mask node-mask))
    (setf (ldb (byte 1 index) node-mask) 1

          (aref content
                (logcount (ldb (byte index 0) node-mask)))
          item)
    (if transactional
        (make-transactional-hash-node :node-mask node-mask
                                      :content content)
        (make-hash-node :node-mask node-mask
                        :content content))))


(-> hash-node-insert-into-copy (hash-node t hash-node-index &key (:transactional boolean)) hash-node)
(declaim (inline hash-node-insert-into-copy))
(defun hash-node-insert-into-copy (hash-node content index &key (transactional nil))
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 0)
                     (space 0)))
  (let ((position (hash-node-to-masked-index hash-node index)))
    (cl-ds.utils:with-vectors
        ((current-array (hash-node-content hash-node))
         (new-array (make-array (1+ (array-dimension current-array 0)))))
      (assert (~> (array-dimension new-array 0)
                  (<= +maximum-children-count+)))
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
      (let ((node-mask (hash-node-node-mask hash-node)))
        (setf (ldb (byte 1 index) node-mask) 1)
        (if transactional
            (make-transactional-hash-node :node-mask node-mask
                                          :content new-array)
            (make-hash-node :node-mask node-mask
                            :content new-array))))))


(-> rebuild-rehashed-node (fixnum t &key (:transactional boolean)) t)
(-> build-rehashed-node (fixnum simple-vector &key (:transactional boolean)) t)
(defun build-rehashed-node (depth content &key (transactional nil))
  (let ((mask 0)
        (node-mask 0)
        (size 0))
    (iterate
      (for elt in-vector content)
      (for index from 0)
      (when elt
        (incf size)
        (setf (ldb (byte 1 index) mask) 1)))
    (cl-ds.utils:with-vectors ((array (make-array size)))
      (iterate
        (for conflict in-vector content)
        (for index from 0)
        (when conflict
          (for i = (logcount (ldb (byte index 0) mask)))
          (setf (array i)
                (rebuild-rehashed-node depth
                                       conflict
                                       :transactional transactional)
                (ldb (byte 1 index) node-mask) 1)))
      (if transactional
          (make-transactional-hash-node :node-mask node-mask
                                        :content array)
          (make-hash-node :node-mask node-mask
                          :content array)))))


(let ((max-mask (iterate
                  (for i from 0 below +maximum-children-count+)
                  (for result
                       initially 0
                       then (dpb 1 (byte 1 i) result))
                  (finally (return result)))))
  (defun mark-everything-as-modified (node)
    (setf (transactional-hash-node-modification-mask node)
          max-mask)
    node))


(defun transactional-rebuild-rehashed-node (depth conflict)
  (declare (optimize (speed 3)))
  (flet ((cont (array)
           (let ((result (build-rehashed-node (1+ depth) array
                                              :transactional t)))
             (mark-everything-as-modified result))))
    (declare (dynamic-extent #'cont))
    (if (or (>= depth +depth+) (cl-ds.common:single-element-p conflict))
        conflict
        (rehash conflict depth
                #'cont))))


(defun rebuild-rehashed-node (depth conflict &key (transactional nil))
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0))
           (type fixnum depth))
  (flet ((cont (array)
           (let ((result (build-rehashed-node (1+ depth)
                                              array
                                              :transactional transactional)))
             (when transactional
               (mark-everything-as-modified result))
             result)))
    (declare (dynamic-extent #'cont))
    (if (or (>= depth +depth+) (cl-ds.common:single-element-p conflict))
        conflict
        (rehash conflict
                depth
                #'cont))))


(-> build-node (hash-node-index t) hash-node)
(defun build-node (index content)
  (make-hash-node :node-mask (ash 1 index)
                  :content (make-array 1 :initial-element content)))


(-> hash-node-insert! (hash-node t hash-node-index) hash-node)
(defun hash-node-insert! (node content index)
  (assert (not (ldb-test (byte 1 index) (hash-node-whole-mask node))))
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
    (cl-ds.utils:with-vectors ((n (make-array next-size)) (s (hash-node-content node)))
      (iterate
        (for i from 0 below next-size)
        (cl-ds.utils:cond-compare (i masked-index)
                                  (setf (n i) (s i))
                                  (setf (n i) content)
                                  (setf (n i) (s (1- i)))))
      (setf (hash-node-content node) n)
      (set-in-node-mask node index 1)
      node)))


(defun hash-node-replace! (node content index)
  (assert (not (zerop (ldb (byte 1 index) (hash-node-whole-mask node)))))
  (cl-ds.utils:with-vectors ((a (hash-node-content node)))
    (setf (a (hash-node-to-masked-index node index))
          content))
  node)


(-> hash-node-remove-from-the-copy (hash-node fixnum &key (:transactional boolean)) hash-node)
(-> hash-node-remove! (hash-node fixnum) hash-node)
(flet ((new-array (node index)
         (cl-ds.utils:copy-without (hash-node-content node)
                                   (1- (logcount (ldb (byte (1+ index) 0)
                                                      (hash-node-whole-mask node)))))))

  (defun hash-node-remove-from-the-copy (node index &key (transactional nil))
    "Returns copy of node, but without element under index. Not safe, does not check if element is actually present."
    (if transactional
        (make-transactional-hash-node :node-mask (dpb 0 (byte 1 index) (hash-node-node-mask node))
                                      :content (new-array node index))
        (make-hash-node :node-mask (dpb 0 (byte 1 index) (hash-node-node-mask node))
                        :content (new-array node index))))


  (defun hash-node-remove! (node index)
    (declare (optimize (debug 0)
                       (speed 3)
                       (space 0)))
    (setf (hash-node-content node)
          (new-array node index))
    (set-in-node-mask node index 0)
    node))


(defun rehash (conflict level cont)
  (declare (type list conflict)
           (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0)))
  (let ((result (make-array +maximum-children-count+ :initial-element nil))
        (byte (byte +hash-level+ (the fixnum
                                      (* (the fixnum +hash-level+)
                                         (the fixnum level))))))
    (declare (dynamic-extent byte)
             (dynamic-extent result))
    (iterate
      (for item in conflict)
      (for hash = (cl-ds.common:hash-content-hash item))
      (for index = (ldb byte hash))
      (push
       item
       (aref result index)))
    (funcall cont result)))


(-> copy-on-write ((vector fixnum) vector fixnum t) t)
(defun copy-on-write (indexes path depth conflict)
  (declare (type index-path indexes)
           (type node-path path)
           (type fixnum depth)
           (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0)))
  (cl-ds.utils:with-vectors (path indexes)
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
       transactional-hash-node-modification-mask
       (ldb-test (byte 1 index))))


(-> set-modified (transactional-hash-node hash-node-index) hash-node)
(defun set-modified (node index)
  (setf (ldb (byte 1 index) (transactional-hash-node-modification-mask node)) 1)
  node)


(-> hash-node-transactional-replace
    (boolean hash-node t hash-node-index) hash-node)
(defun hash-node-transactional-replace (must-copy node content index)
  (let ((result (if must-copy
                    (hash-node-replace-in-the-copy node content index :transactional t)
                    (hash-node-replace! node content index))))
    (when (transactional-hash-node-p result)
      (if (transactional-hash-node-p node)
          (setf (transactional-hash-node-modification-mask result)
                (dpb 1 (byte 1 index) (transactional-hash-node-modification-mask node)))
          (set-modified result index)))
    result))


(-> hash-node-transactional-insert
    (boolean hash-node t hash-node-index)
    hash-node)
(defun hash-node-transactional-insert (must-copy node content index)
  (let ((result (if must-copy
                    (hash-node-insert-into-copy node content index :transactional t)
                    (hash-node-insert! node content index))))
    (when (transactional-hash-node-p result)
      (if (transactional-hash-node-p node)
          (setf (transactional-hash-node-modification-mask result)
                (dpb 1 (byte 1 index) (transactional-hash-node-modification-mask node)))
          (set-modified result index)))
    result))


(-> hash-node-transactional-remove
    (boolean hash-node hash-node-index)
    hash-node)
(defun hash-node-transactional-remove (must-copy node index)
  (let ((result (if must-copy
                    (hash-node-remove-from-the-copy node index :transactional t)
                    (hash-node-remove! node index))))
    (when (transactional-hash-node-p result)
      (if (transactional-hash-node-p node)
          (setf (transactional-hash-node-modification-mask result)
                (dpb 1 (byte 1 index) (transactional-hash-node-modification-mask node)))
          (set-modified result index)))
    result))


(-> transactional-copy-on-write
    ((vector fixnum) vector fixnum t boolean)
    t)
(defun transactional-copy-on-write (indexes
                                    path
                                    depth
                                    conflict
                                    root-already-copied)
  (declare (type (simple-array fixnum) indexes)
           (type simple-array path)
           (type fixnum depth)
           (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0)))
  (cl-ds.utils:with-vectors (path indexes)
    (iterate
      (for i from (- depth 1) downto 0) ;reverse order (starting from deepest node)
      (for node = (path i))
      (for index = (indexes i))
      (for parent = (and (not (zerop i))
                         (path (1- i))))
      (for must-copy =
           (if (null parent)
               (not root-already-copied) ;special case for root
               (or (~> parent transactional-hash-node-p not) ;not in the transactional node
                   (hash-node-content-modified parent ;in transactional node, but this child was not modified
                                               (indexes (1- i))))))
      (for ac initially (if (or (hash-node-p conflict)
                                (null conflict))
                            ;;if we didn't find element or element was found but depth was already maximal,
                            ;;we will just return element, otherwise attempt to divide (rehash) conflicting node into few more
                            conflict
                            (transactional-rebuild-rehashed-node depth
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
(declaim (notinline clear-modification-masks))
(defun clear-modification-masks (node)
  (declare (optimize (speed 3)))
  (iterate
    (for i from 0 below +maximum-children-count+)
    (when (and (hash-node-content-modified node i)
               (hash-node-contains-node node i))
      (clear-modification-masks (hash-node-access node i))))
  (setf (transactional-hash-node-modification-mask node) 0)
  node)


(-> hash-node-deep-copy (hash-node) (values hash-node (or null hash-mask)))
(declaim (inline hash-node-deep-copy))
(defun hash-node-deep-copy (node)
  (if (transactional-hash-node-p node)
      (values (make-hash-node :node-mask (transactional-hash-node-node-mask node)
                              :content (copy-array (transactional-hash-node-content node)))
              (transactional-hash-node-modification-mask node))
      (values (make-hash-node :node-mask (hash-node-node-mask node)
                              :content (copy-array (hash-node-content node)))
              nil)))


(-> isolate-transactional-instance (hash-node boolean) hash-node)
(declaim (notinline isolate-transactional-instance))
(defun isolate-transactional-instance (parent parent-was-modified)
  (multiple-value-bind (parent mask) (if parent-was-modified
                                         (hash-node-deep-copy parent)
                                         (values parent nil))
    (cl-ds.utils:with-vectors ((content (hash-node-content parent)))
      (unless (null mask)
        (iterate
          (with j = 0)
          (for i from 0 below +maximum-children-count+)
          (for was-modified = (and (hash-node-contains-node parent i)
                                   (ldb-test (byte 1 i) mask)))
          (when (and was-modified (hash-node-p (content j)))
            (setf (content j)
                  (isolate-transactional-instance (content j)
                                                  t))
            (incf j)))))
    parent))


(defstruct hamt-range-stack-cell
  (start 0 :type non-negative-fixnum)
  (end 0 :type non-negative-fixnum)
  node)


(defun new-cell (node)
  (cond
    ((consp node) node)
    ((hash-node-p node) (make-hamt-range-stack-cell
                         :start 0
                         :end (1- (hash-node-size node))
                         :node node))
    (t (error "Logic Error!"))))


(defun forward-cell (cell)
  (cond
    ((hamt-range-stack-cell-p cell)
     (values
      (new-cell (~> cell hamt-range-stack-cell-node hash-node-content
                    (aref (hamt-range-stack-cell-start cell))))
      (unless (eql (hamt-range-stack-cell-start cell) (hamt-range-stack-cell-end cell))
        (make-hamt-range-stack-cell
         :start (1+ (hamt-range-stack-cell-start cell))
         :end (hamt-range-stack-cell-end cell)
         :node (hamt-range-stack-cell-node cell)))))
    ((listp cell)
     (values
      (first cell)
      (rest cell)))
    (t (error "Logic Error!"))))


(defun obtain-value (pull push)
  (declare (optimize (debug 3)))
  (iterate
    (for old-cell = (funcall pull))
    (for (values new-cell modified-cell) = (forward-cell old-cell))
    (unless (null modified-cell) ;push if anything
      (funcall push modified-cell))
    (if (listp old-cell)
        (return-from obtain-value (values new-cell t))
        (unless (null new-cell)
          (funcall push new-cell)))))


(defgeneric get-range-key-function (container))


(defmethod cl-ds:whole-range ((container hamt-container))
  (make 'cl-ds.common:forward-tree-range
        :obtain-value #'obtain-value
        :key (get-range-key-function container)
        :forward-stack (list (new-cell (access-root container)))))
