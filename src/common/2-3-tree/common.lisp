(in-package #:cl-data-structures.common.2-3-tree)


(defclass tree ()
  ((%root :initform nil
          :accessor access-root)
   (%size :initform 0
          :accessor access-size
          :reader cl-ds:size)))


(defclass node ()
  ())


(defclass 2-node (node)
  ((%left :initform cl-ds.meta:null-bucket
          :initarg :left
          :accessor access-left)
   (%right :initform cl-ds.meta:null-bucket
           :initarg :right
           :accessor access-right)))


(defclass 3-node (2-node)
  ((%left :initform cl-ds.meta:null-bucket
          :initarg :left
          :accessor access-left)
   (%middle :initform cl-ds.meta:null-bucket
            :initarg :middle
            :accessor access-middle)
   (%right :initform cl-ds.meta:null-bucket
           :initarg :right
           :accessor access-right)))


(defclass tagged-2-node
    (2-node cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ())


(defclass tagged-3-node
    (3-node cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ())


(defgeneric insert-front (node new))


(defgeneric take-back (node))


(defmethod insert-front ((node t) item)
  (values (funcall item) node))


(defmethod insert-front ((node 2-node) new)
  (bind ((left (access-left node))
         ((:values n1 n2) (insert-front left new)))
	  (if (cl-ds.meta:null-bucket-p n2)
		    (let ((new-node (make '2-node :left n1 :right (access-right node))))
			    (values new-node cl-ds.meta:null-bucket))
		    (let ((new-node (make '3-node
                              :left n1
                              :middle n2
                              :right (access-right node))))
		      (values new-node cl-ds.meta:null-bucket)))))


(defun insert-front-into-tree (tree new)
  (bind (((:values n1 n2) (insert-front tree new)))
    (if (cl-ds.meta:null-bucket-p n2)
        n1
        (make '2-node
              :left n1
              :right n2))))


(defmethod insert-front ((node 3-node) new)
  (bind ((left (access-left node))
         ((:values n1 n2) (insert-front left new)))
    (if (cl-ds.meta:null-bucket-p n2)
		    (let ((new-node (make '3-node
                              :left n1
                              :middle (access-middle node)
                              :right (access-right node))))
			    (values new-node cl-ds.meta:null-bucket))
		    (let ((new-node-1 (make '2-node
                                :left n1
                                :right n2))
		          (new-node-2 (make '2-node
                                :left (access-middle node)
                                :right (access-right node))))
		      (values new-node-1 new-node-2)))))


(defgeneric delete-back (tree-or-node))


(defmethod delete-back ((node t))
  (values cl-ds.meta:null-bucket t))


(defmethod delete-back ((node 2-node))
  (bind ((left (access-left node))
         (right (access-right node))
         ((:values n lower-p) (delete-back right)))
	  (cond ((null n)
		       ;; The right child of NODE was a leaf, so it got
		       ;; deleted.  We need to return the left child of
		       ;; NODE, but first we need to make sure it is no
		       ;; longer referenced from NODE.
           (values (access-left node) t))
		      ((not lower-p)
		       ;; This is the simple case where we got back a
		       ;; node with the same height as the original right
		       ;; child.  It suffices to decrement the size of
		       ;; the subtree rooted at NODE, replace the
		       ;; original right child with what we got back, and
		       ;; return the original node.
		       (values (make '2-node
                         :left (access-left node)
                         :right n)
                   nil))
		      ((typep left '2-node)
		       ;; The node N that resulted from the recursive
		       ;; call to delete-back of the right child is the root
		       ;; of a subtree that is lower than the original
		       ;; right child of NODE, and the left child of NODE
		       ;; is a 2-node.  The two children of the left
		       ;; child of NODE and the node N all have the same
		       ;; height, so we stick them in a new 3-node that
		       ;; we return.  The tree rooted at that 3-node is
		       ;; lower than the original tree rooted at NODE.
           (values (make '3-node
                         :left (make '2-node
                                     :right nil
                                     :left nil)
                         :right n
                         :middle (access-right left))
                   t))
		      (t
		       ;; The left child is a 3-node
		       (let ((l (access-left left))
			           (m (access-middle left))
			           (r (access-right left))
			           (new-node-1 (make '2-node))
			           (new-node-2 (make '2-node))
                 (result-new-node (make '2-node)))
		         (setf (access-left new-node-1) l
		               (access-right new-node-1) m
		               (access-left new-node-2) r
		               (access-right new-node-2) n
		               (access-left result-new-node) new-node-1
		               (access-right result-new-node) new-node-2)
		         (values result-new-node nil))))))


(defmethod delete-back ((node 3-node))
  (bind ((left (access-left node))
	       (middle (access-middle node))
	       (right (access-right node))
         ((:values n lower-p)
          (delete-back right)))
    (cond ((null n)
		       ;; The right child of NODE was a leaf, so it got
		       ;; deleted.  We now have two children left, so we
		       ;; replace the original 3-node by a 2-node,
		       ;; holding the remaining children.
		       (values (make '2-node
                         :left left
                         :middle middle)
                   nil))
		      ((not lower-p)
		       ;; This is the simple case where we got back a
		       ;; node with the same height as the original right
		       ;; child.  It suffices to decrement the size of
		       ;; the subtree rooted at NODE, replace the
		       ;; original right child with what we got back, and
		       ;; return the original node.
		       (values (make '3-node
                         :right n
                         :left left
                         :middle middle)
                   nil))
		      ((typep middle '2-node)
		       ;; The node n represents a subtree that has the
		       ;; same height as the children of the 2-node in
		       ;; the middle child of the 3-node NODE.  We put n
		       ;; and the children of the 2-node in a new 3-node,
		       ;; and we replace NODE by a 2-node with the old
		       ;; left child of NODE and the new 3-node as
		       ;; children.
		       (bind ((l (access-left middle))
			            (r (access-right middle))
			            (new-node-1 (make '3-node))
			            (new-node-2 (make '2-node)))
		         (setf (access-left new-node-1) l
		               (access-middle new-node-1) r
		               (access-right new-node-1) n
		               (access-left new-node-2) left
		               (access-right new-node-2) new-node-1)
		         (values new-node-2 nil)))
		      (t
		       ;; The node n represents a subtree that has the
		       ;; same height as the children of the 3-node in
		       ;; the middle child of the 3-node NODE.  We
		       ;; redistribute the return value and the three
		       ;; children of the middle sibling (4 objects in
		       ;; total) as the children of two 2-nodes.
		       (bind ((l (access-left middle))
			            (m (access-middle middle))
			            (r (access-right middle))
			            (new-node-1 (make '2-node))
			            (new-node-2 (make '2-node))
                  (node (make '3-node
                              :left (access-left node)
                              :middle new-node-1
                              :right new-node-2)))
		         (setf (access-left new-node-1) l
		               (access-right new-node-1) m
		               (access-left new-node-2) r
		               (access-right new-node-2) n)
		         (values node nil))))))
