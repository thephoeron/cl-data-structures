;; link to java implementation https://github.com/clojure/clojure/blob/0b73494c3c855e54b1da591eeb687f24f608f346/src/jvm/clojure/lang/PersistentVector.java
(in-package #:cl-data-structures.common.rrb)


(define-constant +bit-count+ 5)
(define-constant +maximal-shift+ (iterate
                                   (for c
                                        initially most-positive-fixnum
                                        then (ash c (- +bit-count+)))
                                   (until (zerop c))
                                   (counting t)))
(define-constant +maximum-children-count+ (ash 1 +bit-count+))
(define-constant +tail-mask+ (dpb 0 (byte +bit-count+ 0) most-positive-fixnum))

(deftype node-content ()
  `(simple-vector ,+maximum-children-count+))
(deftype node-size ()
  `(integer 0 ,+maximum-children-count+))
(deftype shift ()
  `(integer 0 ,+maximal-shift+))


(defun make-node-content ()
  (make-array +maximum-children-count+ :initial-element nil))


(defstruct (rrb-node (:include tagged-node))
  (content (make-node-content) :type node-content))


(defmethod print-object ((obj rrb-node) stream)
  (format stream "<")
  (iterate
    (for elt in-vector (rrb-node-content obj))
    (for p-elt previous elt)
    (until (null elt))
    (unless (null p-elt)
      (format stream ", "))
    (format stream "~a" elt))
  (format stream ">"))


(-> rrb-node-deep-copy (rrb-node list) rrb-node)
(declaim (notinline rrb-node-deep-copy))
(defun rrb-node-deep-copy (node ownership-tag)
  (make-rrb-node :ownership-tag ownership-tag
                 :content (copy-array (rrb-node-content node))))


(defun rrb-node-push! (node position element)
  (setf (aref (rrb-node-content node) position) element)
  node)


(defun rrb-node-push-into-copy (node position element ownership-tag)
  (let ((result-content (make-array +maximum-children-count+
                                    :initial-element nil))
        (source-content (rrb-node-content node)))
    (setf (aref result-content position) element)
    (iterate
      (for i from 0 below position)
      (setf (aref result-content i) (aref source-content i)))
    (make-rrb-node :ownership-tag ownership-tag
                   :content result-content)))


(defun rrb-node-pop-in-the-copy (node position ownership-tag)
  (declare (optimize (debug 3)))
  (unless (zerop position)
    (let* ((source-content (rrb-node-content node))
           (result-content (copy-array source-content)))
      (setf (aref result-content position) nil)
      (make-rrb-node :ownership-tag ownership-tag
                     :content result-content))))


(defun rrb-node-pop! (node position)
  (setf (aref (rrb-node-content node) position) nil)
  node)


(defclass rrb-container (fundamental-ownership-tagged-object)
  ((%root :accessor access-root
          :initarg :root
          :initform nil
          :type (or rrb-node nil)
          :documentation "root of the tree")
   (%shift :initarg :shift
           :accessor access-shift
           :type shift
           :initform 0)
   (%size :initarg :size
          :initform 0
          :type non-negative-fixnum
          :accessor access-size)
   (%tail-size :initform 0
               :initarg :tail-size
               :type node-size
               :accessor access-tail-size)
   (%tail :initform nil
          :type (or null simple-array)
          :initarg :tail
          :accessor access-tail)))


(declaim (inline tail-offset))
(-> tail-offset (non-negative-fixnum) non-negative-fixnum)
(defun tail-offset (size)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (if (< size 32)
      0
      (~> size 1- (logand +tail-mask+))))


(defun push-into-copy-of-tail (rrb-container ownership-tag element)
  (bind (((:slots %tail-size %tail) rrb-container))
    (unless (eql %tail-size +maximum-children-count+)
      (bind ((content (rrb-node-content %tail))
             ((:vectors copy-of-content) (copy-array content)))
        (setf (copy-of-content %tail-size) element)
        (make-rrb-node :ownership-tag ownership-tag
                       :content copy-of-content)))))


(defun push-into-tail! (rrb-container element)
  (bind (((:slots %tail-size %tail) rrb-container))
    (unless (eql %tail-size +maximum-children-count+)
      (bind (((:vectors content) (rrb-node-content %tail)))
        (setf (content %tail-size) element)
        t))))


(declaim (notinline insert-tail))
(-> insert-tail (rrb-container
                 t
                 function
                 node-content)
    rrb-node)
(defun insert-tail (rrb-container ownership-tag continue tail)
  (declare (optimize (debug 3)))
  (bind (((:slots %size %shift %root) rrb-container)
         (root-overflow (>= (the fixnum (ash (the fixnum %size) (- +bit-count+)))
                            (ash 1 (* +bit-count+ (the shift %shift))))))
    (if root-overflow
        (iterate
          (repeat %shift)
          (for node
               initially (make-rrb-node :content tail
                                        :ownership-tag ownership-tag)
               then (let ((next (make-rrb-node :ownership-tag ownership-tag)))
                      (setf (~> next rrb-node-content (aref 0))
                            node)
                      next))
          (finally
           (bind ((root (make-rrb-node :ownership-tag ownership-tag))
                  ((:vectors content) (rrb-node-content root)))
             (setf (content 0) %root
                   (content 1) node)
             (return (values root t)))))
        (let ((path (make-array +maximal-shift+
                                :initial-element nil))
              (indexes (make-array +maximal-shift+
                                   :element-type `(integer 0 ,+maximum-children-count+))))
          (declare (dynamic-extent path)
                   (dynamic-extent indexes))
          (iterate
            (with size = (the non-negative-fixnum %size))
            (for i from 0 below %shift)
            (for position from (* +bit-count+ %shift) downto 0 by +bit-count+)
            (for index = (ldb (byte +bit-count+ position) size))
            (for node
                 initially %root
                 then (and node (~> node rrb-node-content (aref index))))
            (setf (aref path i) node
                  (aref indexes i) index))
          (values (funcall continue
                           path
                           indexes
                           %shift
                           ownership-tag
                           tail)
                  nil)))))


(defun rrb-at (container index)
  (declare (optimize (debug 3))
           (type non-negative-fixnum index)
           (type rrb-container container))
  (unless (> (cl-ds:size container) index)
    (error 'cl-ds:argument-out-of-bounds
           :bounds (list 0 (cl-ds:size container))
           :value index
           :text "Index out of bounds."))
  (if (< index (access-size container))
      (iterate
        (with shift = (slot-value container '%shift))
        (for position from (* +bit-count+ shift) downto 0 by +bit-count+)
        (for i = (ldb (byte +bit-count+ position) index))
        (for node
             initially (slot-value container '%root)
             then (~> node rrb-node-content (aref i)))
        (finally (return node)))
      (let ((offset (- index (access-size container))))
        (~> container access-tail (aref offset)))))


(defmethod cl-ds:at ((container rrb-container) index)
  (check-type index non-negative-fixnum)
  (rrb-at container index))


(-> copy-on-write (t t t t t) t)
(defun copy-on-write (path indexes shift ownership-tag tail)
  (declare (optimize (debug 3)))
  (iterate
    (for i from (1- shift) downto 0)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         initially (make-rrb-node :content tail
                                  :ownership-tag ownership-tag)
         then (if (null old-node)
                  (let ((n (make-rrb-node :ownership-tag ownership-tag)))
                    (setf (~> n rrb-node-content (aref position)) node)
                    n)
                  (rrb-node-push-into-copy old-node
                                           position
                                           node
                                           ownership-tag)))
    (finally (return node))))


(defun acquire-path (path shift ownership-tag)
  (or (iterate
        (for i from 0 below shift)
        (for node = (aref path i))
        (finding i such-that (~> node
                                 (acquire-ownership ownership-tag)
                                 null)))
      shift))


(defun transactional-copy-on-write (path indexes shift ownership-tag tail)
  (iterate
    (with acquired = (acquire-path path shift ownership-tag))
    (for i from (1- shift) downto 0)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         initially (make-rrb-node :content tail
                                  :ownership-tag ownership-tag)
         then (if (null old-node)
                  (let ((n (make-rrb-node :ownership-tag ownership-tag)))
                    (setf (~> n rrb-node-content (aref position)) node)
                    n)
                  (if (< i acquired)
                      (rrb-node-push! old-node
                                      position
                                      node)
                      (rrb-node-push-into-copy old-node
                                               position
                                               node
                                               ownership-tag))))
    (finally (return node))))


(defun destructive-write (path indexes shift ownership-tag tail)
  (iterate
    (for i from 0 below shift)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         initially (make-rrb-node :content tail
                                  :ownership-tag ownership-tag)
         then (if (null old-node)
                  (let ((n (make-rrb-node :ownership-tag ownership-tag)))
                    (setf (~> n rrb-node-content (aref position)) node)
                    n)
                  (rrb-node-push! old-node position node)))
    (finally (return node))))


(defun remove-tail (rrb-container ownership-tag continue)
  (bind (((:slots %size %shift %root) rrb-container)
         (first-index (* +bit-count+ %shift))
         (root-size (ldb (byte +bit-count+ first-index)
                         %size))
         (root-underflow (eql root-size 1))
         (path (make-array +maximal-shift+
                           :initial-element nil))
         (indexes (make-array +maximal-shift+
                              :element-type `(integer 0 ,+maximum-children-count+))))
    (declare (dynamic-extent path indexes))
    (assert (> %shift 0))
    (if root-underflow
        (if (eql 1 %shift)
            (values nil t)
            (values (~> %root rrb-node-content (aref 0))
                    t))
        (iterate
          (with size = (the non-negative-fixnum %size))
          (for i from 0 below %shift)
          (for position
               from first-index
               downto 0
               by +bit-count+)
          (for index = (ldb (byte +bit-count+ position) (1- size)))
          (for node
               initially %root
               then (and node (~> node rrb-node-content (aref index))))
          (setf (aref path i) node
                (aref indexes i) index)
          (finally (return (values (funcall continue
                                            path
                                            indexes
                                            %shift
                                            ownership-tag)
                                   nil)))))))


(defun copy-on-write-without-tail (path indexes shift ownership-tag)
  (declare (optimize (debug 3)))
  (iterate
    (for i from (1- shift) downto 0)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         first (rrb-node-pop-in-the-copy old-node
                                         position
                                         ownership-tag)
         then (rrb-node-push-into-copy old-node
                                       position
                                       node
                                       ownership-tag))
    (finally (return node))))


(defun transactional-copy-on-write-without-tail (path indexes shift ownership-tag)
  (declare (optimize (debug 3)))
  (bind ((acquired (acquire-path path shift ownership-tag))
         ((:dflet push-impl (old-node position node ownership-tag i))
          (if (< i acquired)
              (rrb-node-push! old-node
                              position
                              node)
              (rrb-node-push-into-copy old-node
                                       position
                                       node
                                       ownership-tag)))
         ((:dflet pop-impl (old-node position ownership-tag i))
          (if (< i acquired)
              (rrb-node-pop! old-node position)
              (rrb-node-pop-in-the-copy old-node position ownership-tag))))
    (iterate
      (for i from (1- shift) downto 0)
      (for position = (aref indexes i))
      (for old-node = (aref path i))
      (for node
           first (pop-impl old-node position ownership-tag i)
           then (push-impl old-node position node ownership-tag i))
      (finally (return node)))))


(defun destructive-write-without-tail (path indexes shift ownership-tag)
  (declare (optimize (debug 3))
           (ignore ownership-tag))
  (iterate
    (for i from (1- shift) downto 0)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         first (rrb-node-pop! old-node
                              position)
         then (rrb-node-push! old-node
                              position
                              node))
    (for p-node previous node initially t)
    (until (null p-node))
    (finally (return node))))


(defmethod cl-ds:size ((container rrb-container))
  (+ (access-size container)
     (access-tail-size container)))
