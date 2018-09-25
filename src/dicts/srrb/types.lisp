(in-package #:cl-data-structures.dicts.srrb)


(defclass fundamental-sparse-rrb-vector
    (cl-ds.dicts:fundamental-sparse-vector)
  ((%tree :initarg :tree
          :initform nil
          :accessor access-tree)
   (%tail :initarg :tail
          :accessor access-tail
          :initform nil)
   (%shift :initarg :shift
           :accessor access-shift
           :initform 0)
   (%tree-size :initarg :tree-size
               :accessor access-tree-size
               :initform 0)
   (%tree-index-bound :initarg :tree-index-bound
                      :initform 0
                      :accessor access-tree-index-bound)
   (%index-bound :initarg :index-bound
                 :accessor access-index-bound
                 :initform 0)
   (%tail-mask :initarg :tail-mask
               :initform 0
               :accessor access-tail-mask)
   (%element-type :initarg :element-type
                  :reader read-element-type
                  :initform t)))


(defclass mutable-sparse-rrb-vector (fundamental-sparse-rrb-vector
                                     cl-ds:mutable)
  ())


(defun insert-tail (structure)
  cl-ds.utils:todo)


(defun insert-tail! (structure)
  (let ((new-root (insert-tail structure)))
    (setf (access-tree structure) new-root
          (access-tree-index-bound structure) (access-index-bound structure)))
  structure)


(defun adjust-tree-to-new-size! (structure position)
  cl-ds.utils:todo)


(defun set-new-tail! (structure container position value)
  cl-ds.utils:todo)


(defun set-in-tail! (structure operation container position offset value all)
  (bind ((tail (access-tail structure))
         (tail-mask (access-tail-mask structure))
         ((:values bucket status changed)
          (apply #'cl-ds.meta:make-bucket operation container value all)))
    (when changed
      (setf (aref tail offset) bucket
            (access-tail-mask structure) (dpb 1 (byte 1 offset) tail-mask)
            (access-index-bound structure) position))
    (values structure status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure mutable-sparse-rrb-vector)
                                             container
                                             position &rest all &key value)
  (let ((tree-bound (access-tree-index-bound structure)))
    (cond ((negative-fixnum-p position)
           cl-ds.utils:todo) ; should signal proper error if position is negative
          ((< position tree-bound)
           (iterate
             (with tree = (access-tree structure))
             (with shift = (access-shift structure))
             (with all-exist = t)
             (for position
                  from (* cl-ds.common.rrb:+bit-count+
                          shift)
                  downto 0
                  by cl-ds.common.rrb:+bit-count+)
             (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ position)
                           position))
             (for p-i previous i)
             (for node
                  initially tree
                  then (cl-ds.common.rrb:sparse-nref node i))
             (for present =
                  (cl-ds.common.rrb:sparse-rrb-node-contains node i))
             (unless present
               (setf all-exist nil)
               (let* ((old-content (cl-ds.common.rrb:sparse-rrb-node-content node))
                      (old-content-size (array-dimension old-content 0))
                      (old-bitmask (cl-ds.common.rrb:sparse-rrb-node-bitmask node))
                      (new-bitmask (dpb 1 (byte 1 i) old-bitmask))
                      (splice-index (logcount (ldb (byte i 0) new-bitmask)))
                      (old-count (logcount old-bitmask))
                      (new-count (1+ old-count))
                      (new-content (if (<= new-count old-content-size)
                                       old-content
                                       (make-array new-count
                                                   :element-type (array-element-type old-content)))))
                 (unless (eq old-content new-content)
                   (setf (cl-ds.common.rrb:sparse-rrb-node-content node) new-content)
                   (iterate
                     (for i from 0 below splice-index)
                     (setf (aref new-content i) (aref old-content i))))
                 (iterate
                   (for i from splice-index below old-count)
                   (setf (aref new-content (1+ i)) (aref old-content i)))
                 (setf (aref new-content splice-index)
                       (cl-ds.common.rrb:make-sparse-rrb-node
                        :content (make-array 1 :element-type (array-element-type old-content)))

                       (cl-ds.common.rrb:sparse-rrb-node-bitmask node) new-bitmask)))
             (finally
              (if all-exist
                  cl-ds.utils:todo ; should modify bucket
                  (cl-ds.common.rrb:with-sparse-rrb-node node
                    (bind (((:values new-bucket status changed)
                            (apply #'cl-ds.meta:make-bucket
                                   operation container
                                   value all)))
                      (when changed
                        (setf (cl-ds.common.rrb:sparse-nref node i) new-bucket)
                        (incf (access-tree-size structure)))
                      (values structure status)))))))
          (t (let* ((offset (- position tree-bound)))
               (if (< offset cl-ds.common.rrb:+maximum-children-count+)
                   (set-in-tail! structure operation position
                                 container offset value all)
                   (progn
                     (insert-tail! structure)
                     (adjust-tree-to-new-size! structure position)
                     (set-new-tail! structure container position value))))))))


(defmethod cl-ds:size ((vect fundamental-sparse-rrb-vector))
  (+ (access-tree-size vect) (logcount (access-tail-mask vect))))


(defmethod cl-ds:at ((vect fundamental-sparse-rrb-vector)
                     position
                     &rest more-positions)
  (cl-ds:assert-one-dimension more-positions)
  (check-type position fixnum)
  (let ((bound (access-index-bound vect))
        (tree-bound (access-tree-index-bound vect)))
    (cond ((not (< -1 position bound))
           (values nil nil))
          ((< position tree-bound)
           (iterate
             (with tree = (access-tree vect))
             (with shift = (access-shift vect))
             (for position
                  from (* cl-ds.common.rrb:+bit-count+
                          shift)
                  downto 0
                  by cl-ds.common.rrb:+bit-count+)
             (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ position)
                           position))
             (for node initially tree
                  then (cl-ds.common.rrb:sparse-nref node i))
             (for present =
                  (cl-ds.common.rrb:sparse-rrb-node-contains node
                                                             i))
             (unless present
               (leave (values nil nil)))
             (finally (return (values node t)))))
          (t (let* ((offset (- position tree-bound))
                    (present (ldb-test (byte 1 offset)
                                       (access-tail-mask vect))))
               (if present
                   (values (aref (access-tail vect) offset) t)
                   (values nil nil)))))))
