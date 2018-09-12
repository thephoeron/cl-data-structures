(in-package #:cl-data-structures.common.2-3-tree)


(defclass 2-3-node ()
  ())


(defclass fundamental-finger-tree
    (cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ((%root :initarg :root
          :initform nil
          :accessor access-root
          :type finger-tree-node)))


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


(defclass tagged-2-node
    (2-node cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ())


(defclass tagged-3-node
    (3-node cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ())


(defclass tagged-1-content
    (1-content cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ())


(defclass tagged-2-content
    (2-content cl-ds.common.abstract:fundamental-ownership-tagged-object)
  ())


(defgeneric insert-front (new node))


(defgeneric take-back (node))


(defmethod insert-front (new (node 1-content))
  (make '2-content
        :content-1 (funcall new)
        :content-2 (access-content-1 node)))


(defmethod take-back ((node 2-content))
  (values (make '1-content :content-1 (access-content-1 node))
          node))


(defmethod take-back ((node 2-node))
  (bind ((right (access-right node))
         ((:values new-node old-node) (take-back right)))
    (if (null new-node)
        (values nil old-node)
        (values (make '2-node :content-1 (access-content-1 node)
                              :left (access-left node)
                              :right new-node)
                old-node))))


(defmethod take-back ((node 3-node))
  (bind ((right (access-right node))
         ((:values new-node old-node) (take-back right)))
    (if (null new-node)
        (let ((center (access-center node)))
          (if (typep center '2-content)
              (values (make '3-node
                            :content-1 (access-content-1 node)
                            :content-2 (access-content-2 center)
                            :left (access-left node)
                            :center (make '1-content
                                          :content-1 (access-content-1 center))
                            :right (make '1-content
                                         :content-1 (access-content-2 node)))
                      old-node)
              (values (make '2-node
                            :left (access-left node)
                            :content-1 (access-content-1 center)
                            :right (make '2-content
                                         :content-1 (access-content-1 node)
                                         :content-2 (access-content-2 node)))
                      old-node)))
        (values (make '3-node :content-1 (access-content-1 node)
                              :content-2 (access-content-2 node)
                              :left (access-left node)
                              :center (access-center node)
                              :right new-node)
                old-node))))


(defmethod insert-front (new (node (eql nil)))
  (make '1-CONTENT
        :content-1 (funcall new)))


(defmethod insert-front (new (node 2-content))
  nil)


(defun insert-front-handle-nil (new-node old-node new)
  (if (null new-node)
      (progn
        (check-type old-node 2-content)
        (make '2-node
              :left (make '1-content :content-1 (funcall new))
              :content-1 (access-content-1 old-node)
              :right (make '1-content :content-1 (access-content-2 old-node))))
      new-node))


(defun take-back-handle-nil (new-node old-node)
  (if (null new-node)
      (if (typep old-node '1-content)
          nil
          (let ((left (access-left old-node)))
            (if (typep left '1-content)
                (make '2-content
                      :content-1 (access-content-1 old-node)
                      :content-2 (access-content-1 left))
                (make '2-node
                      :content-1 (access-content-2 left)
                      :left (make '1-content
                                  :content-1 (access-content-1 left))
                      :right (make '1-content
                                   :content-1 (access-content-1 old-node))))))
      new-node))


(defmethod insert-front (new (node 2-node))
  (let* ((left (access-left node))
         (result (insert-front new (access-left node))))
    (if (null result)
        (make-instance
         '3-node
         :left (make '1-content :content-1 (funcall new))
         :content-1 (access-content-1 left)
         :content-2 (access-content-1 node)
         :center (make '1-content :content-1 (access-content-2 left))
         :right (access-right node))
        (make-instance
         '2-node
         :left result
         :content-1 (access-content-1 node)
         :right (access-right node)))))


(defmethod insert-front (new (node 3-node))
  (let* ((left (access-left node))
         (result (insert-front new (access-left node))))
    (if (null result)
        (make-instance
         '2-node
         :left (make '2-node
                     :left (make '1-content :content-1 (funcall new))
                     :content-1 (access-content-1 left)
                     :right (make '1-content :content-1 (access-content-2 left)))
         :content-1 (access-content-1 node)
         :right (make '2-node
                      :left (access-center node)
                      :content-1 (access-content-2 node)
                      :right (access-right node)))
        (make-instance
         '3-node
         :left result
         :content-1 (access-content-1 node)
         :content-2 (access-content-2 node)
         :center (access-center node)
         :right (access-right node)))))
