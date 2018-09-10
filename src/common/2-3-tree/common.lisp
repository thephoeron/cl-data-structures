(in-package #:cl-data-structures.common.2-3-tree)


(defclass 2-3-node ()
  ())


(defgeneric 2-3-node-p (object)
  (:method ((object 2-3-node))
    t)
  (:method (object)
    nil))


(defclass 1-content ()
  ((%content-1 :initarg :content-1
               :type content-1
               :accessor access-content-1)))


(defclass 2-content (1-content)
  ((%content-2 :initarg :content-2
               :type content-2
               :accessor access-content-2)))


(defclass 2-node (2-3-node 1-content)
  ((%left :initarg :left
          :accessor access-left)
   (%right :initarg :right
          :accessor access-right)))


(defclass 3-node (2-node 2-content)
  ((%center :initarg :center
            :accessor access-center)))


(defgeneric insert-front (new node overflow-buffer))


(defmethod insert-front (new (node 1-content) overflow-buffer)
  (make '2-content
        :content-1 (funcall new)
        :content-2 (access-content-1 node)))


(defmethod insert-front (new (node 2-content) overflow-buffer)
  (declare (type overflow-buffer overflow-buffer))
  nil)


(defmethod insert-front (new (node 2-node) overflow-buffer)
  (let* ((left (access-left node))
         (result (insert-front new (access-left node) overflow-buffer)))
    (if (null result)
      (make-instance '3-node
                     :left (make '1-content :content-1 (funcall new))
                     :content-1 (access-content-1 left)
                     :content-2 (access-content-1 node)
                     :center (make '1-content :content-1 (access-content-2 left))
                     :right (access-right node))
      (make-instance '2-node
                     :left result
                     :content-1 (access-content-1 node)
                     :right (access-right node)))))


(defmethod insert-front (new (node 3-node) overflow-buffer)
  (let* ((left (access-left node))
         (result (insert-front new (access-left node) overflow-buffer)))
    (if (null result)
      (make-instance '2-node
                     :left (make '2-node
                                 :left (make '1-content :content-1 (funcall new))
                                 :content-1 (access-content-1 left)
                                 :right (make '1-content :content-1 (access-content-2 left)))
                     :content-1 (access-content-1 node)
                     :right(make '2-node
                                 :left (access-center node)
                                 :content-1 (access-content-2 node)
                                 :right (access-right node)))
      (make-instance '3-node
                     :left result
                     :content-1 (access-content-1 node)
                     :content-2 (access-content-2 node)
                     :center (access-center node)
                     :right (access-right node)))))



(defclass tagged-2-node
    (2-node cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ())


(defclass tagged-3-node
    (3-node cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ())


(defun full-3-node (node)
  (macrolet ((all-boundp (&body forms)
               `(and ,@(mapcar (lambda (x) `(slot-boundp node ',x))
                               forms))))
    (all-boundp %left %right %center %content-1 %content-2)))


(defclass fundamental-finger-tree (cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ((%root :initarg :root
          :initform nil
          :accessor access-root
          :type finger-tree-node)))


(defun push-front (new tree)
  (let ((overflow-buffer #(nil nil nil nil)))
    (declare (dynamic-extent overflow-buffer))
    (labels ((push-front-3-node (node)
               ())
             (impl (node)
               (bind (((:values node overflow)
                       (typecase node
                         (2-node
                          (if (slot-boundp node '%right)
                              (values
                               (make '3-node
                                     :left (funcall new)
                                     :content-1 (access-right node)
                                     :content-2 (access-content node))
                               nil)
                              (values
                               (make '2-node
                                     :left (funcall new)
                                     :content (access-left node)
                                     :right (access-content node))
                               nil)))
                         (3-node (push-front-3-node node))
                         (t (values (make '2-node
                                          :left (funcall new)
                                          :content node))))))))))))
