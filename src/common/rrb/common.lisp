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


(defstruct (rrb-node (:include tagged-node))
  (content (make-array +maximum-children-count+) :type node-content))


(-> rrb-node-deep-copy (rrb-node list) rrb-node)
(declaim (inline rrb-node-deep-copy))
(defun rrb-node-deep-copy (node ownership-tag)
  (make-rrb-node :ownership-tag ownership-tag
                 :content (copy-array (rrb-node-content node))))


(defun rrb-node-push! (node position element)
  (setf (aref (rrb-node-content node) position) element)
  node)


(defun rrb-node-push-into-copy (node position element ownership-tag)
  (let ((result-content (make-array +maximum-children-count+))
        (source-content (rrb-node-content node)))
    (setf (aref result-content position) element)
    (iterate
      (for i from 0 below position)
      (setf (aref result-content i) (aref source-content i)))
    (make-rrb-node :ownership-tag ownership-tag
                   :content result-content)))


(defun rrb-node-pop-in-the-copy (node position ownership-tag)
  (let* ((source-content (rrb-node-content node))
         (result-content (copy-array source-content)))
    (setf (aref result-content position) nil)
    (make-rrb-node :ownership-tag ownership-tag
                   :content result-content)))


(defun rrb-node-pop! (node position)
  (setf (aref (rrb-node-content node) position) nil))


(defclass rrb-container (fundamental-ownership-tagged-object)
  ((%root :accessor access-root
          :initarg :root
          :type (or null rrb-node)
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
               :type node-size
               :accessor access-tail-size)
   (%tail :initform nil
          :type (or null rrb-node)
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

(-> insert-tail (rrb-container
                 t
                 (-> ((or null rrb-node) node-size rrb-node t) rrb-node)
                 rrb-node) rrb-node)
(defun insert-tail (rrb-container ownership-tag insert-function tail)
  (declare (optimize (speed 3)))
  (bind (((:slots %size %shift %root) rrb-container)
         (root-overflow (> (the fixnum (ash (the fixnum %size) (- +bit-count+)))
                           (the fixnum (ash 1 (the shift %shift))))))
    (if root-overflow
        (bind ((new-node (iterate
                           (repeat (the non-negative-fixnum %shift))
                           (for node
                                initially tail
                                then (let ((next (make-rrb-node :ownership-tag ownership-tag)))
                                       (setf (~> node rrb-node-content (aref 0)) node)
                                       next))
                           (finally (return node)))))
          (if (null %root)
              (values new-node t)
              (bind ((root (make-rrb-node :ownership-tag ownership-tag))
                     ((:vectors content) (rrb-node-content root)))
                (setf (content 0) %root
                      (content 1) new-node)
                (values root t))))
        (let ((path (make-array `(,+maximum-children-count+ 2))))
          (declare (dynamic-extent path))
          (iterate
            (with size = (1- (the non-negative-fixnum %size)))
            (for i from %shift downto 0)
            (for position from 0 by 5)
            (for index = (ldb (byte +bit-count+ position) size))
            (for node
                 initially %root
                 then (and node (~> node rrb-node-content (aref index))))
            (setf (aref path i 0) node
                  (aref path i 1) index))
          (iterate
            (for i from 0 to %shift)
            (for existing-node = (aref path i 0))
            (for index = (the node-size (aref path i 1)))
            (for next-node
                 initially (funcall insert-function
                                    existing-node
                                    index
                                    tail
                                    ownership-tag)
                 then (funcall insert-function
                               existing-node
                               index
                               next-node
                               ownership-tag))
            (finally (return (values next-node nil))))))))
