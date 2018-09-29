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
                 :initform cl-ds.common.rrb:+maximum-children-count+)
   (%tail-mask :initarg :tail-mask
               :initform 0
               :accessor access-tail-mask)
   (%element-type :initarg :element-type
                  :reader read-element-type
                  :initform t)))


(defclass mutable-sparse-rrb-vector (fundamental-sparse-rrb-vector
                                     cl-ds:mutable)
  ())


(defun insert-tail-handle-root-overflow (shift tree new-node ownership-tag)
  (iterate
    (repeat shift)
    (for node
         initially new-node
         then (cl-ds.common.rrb:make-sparse-rrb-node
               :content (make-array 1 :initial-element node)
               :bitmask 1
               :ownership-tag ownership-tag))
    (finally
     (bind ((content (make-array 2))
            (root (cl-ds.common.rrb:make-sparse-rrb-node
                   :content content
                   :bitmask #b11
                   :ownership-tag ownership-tag)))
       (setf (aref content 0) tree
             (aref content 1) node)
       (return root)))))


(defun make-node-from-tail (rrb-container ownership-tag)
  (bind (((:slots %tree-size %shift %tree %tail %tail-mask
                  %element-type %tree-index-bound)
          rrb-container)
         (tail-mask %tail-mask)
         (tail-size (logcount tail-mask))
         (tail %tail)
         (element-type (array-element-type tail))
         (new-content
          (if (eql tail-size cl-ds.common.rrb:+maximum-children-count+)
              tail
              (iterate
                (with result = (make-array tail-size :element-type element-type))
                (with j = 0)
                (for i from 0 below cl-ds.common.rrb:+maximum-children-count+)
                (for present = (ldb-test (byte 1 i) tail-mask))
                (when present
                  (setf (aref result j) (aref tail i))
                  (incf j))
                (finally (return result)))))
         (new-node (cl-ds.common.rrb:make-sparse-rrb-node
                    :content new-content
                    :bitmask tail-mask
                    :ownership-tag ownership-tag)))
    new-node))


(defun insert-into-node! (into new-element index)
  (let* ((content (cl-ds.common.rrb:sparse-rrb-node-content into))
         (bitmask (cl-ds.common.rrb:sparse-rrb-node-bitmask into))
         (new-bitmask (dpb 1 (byte 1 index) bitmask))
         (length (length content))
         (position (logcount (ldb (byte index 0) new-bitmask)))
         (new-content
           (if (eql length (logcount bitmask))
               (lret ((r (make-array
                          (1+ length)
                          :element-type (array-element-type content))))
                 (setf (cl-ds.common.rrb:sparse-rrb-node-content into) r))
               content)))
    (setf (cl-ds.common.rrb:sparse-rrb-node-bitmask into)
          new-bitmask)
    (iterate
      (for i from 0 below position)
      (setf (aref new-content i) (aref content i)))
    (iterate
      (for i from position below (logcount bitmask))
      (setf (aref new-content (1+ i)) (aref content i)))
    (setf (aref new-content position) new-element)
    new-element))


(defun insert-tail! (structure ownership-tag)
  (declare (optimize (debug 3)))
  (let ((tail-mask (access-tail-mask structure)))
    (unless (zerop tail-mask)
      (bind ((new-node (make-node-from-tail structure ownership-tag))
             ((:accessors (tree access-tree)
                          (tree-size access-tree-size)
                          (%shift access-shift)
                          (tree-index-bound access-tree-index-bound))
              structure)
             (root tree)
             (shift %shift))
        (cond ((null root)
               (setf tree new-node))
              ((>= (the fixnum (ash (the fixnum tree-index-bound)
                                    (- cl-ds.common.rrb:+bit-count+)))
                   (ash 1 (* cl-ds.common.rrb:+bit-count+ shift))) ; overflow
               (let ((new-root (insert-tail-handle-root-overflow
                                shift root new-node ownership-tag)))
                 (incf %shift)
                 (setf tree new-root)))
              (t (iterate
                   (with size = (access-tree-index-bound structure))
                   (with node = root)
                   (with position = (* cl-ds.common.rrb:+bit-count+ shift))
                   (with p-node = nil)
                   (for index = (ldb (byte cl-ds.common.rrb:+bit-count+ position)
                                     size))
                   (for present = (cl-ds.common.rrb:sparse-rrb-node-contains
                                   node index))
                   (shiftf p-node
                           node
                           (and present
                                (cl-ds.common.rrb:sparse-nref node index)))
                   (when (zerop (decf shift))
                     (finish))
                   (decf position cl-ds.common.rrb:+bit-count+)
                   (unless present
                     (let* ((new-element (cl-ds.common.rrb:make-sparse-rrb-node
                                          :ownership-tag ownership-tag
                                          :content (make-array 1))))
                       (insert-into-node! p-node new-element index)
                       (setf node new-element
                             p-node node)))
                   (finally
                    (insert-into-node! p-node new-node index)
                    (setf (access-tail structure) nil)))))))
    (setf (access-tail-mask structure) 0
          (access-tree-index-bound structure) (access-index-bound structure))
    (incf (access-index-bound structure) cl-ds.common.rrb:+maximum-children-count+)
    (incf (access-tree-size structure) (logcount tail-mask)))
  structure)


(defun adjust-tree-to-new-size! (structure position)
  cl-ds.utils:todo)


(-> set-in-tail! (mutable-sparse-rrb-vector
                  cl-ds.meta:grow-function t
                  cl-ds.common.rrb:rrb-node-position
                  t
                  list)
    (values mutable-sparse-rrb-vector t))
(defun set-in-tail! (structure operation container offset value all)
  (bind (((:accessors (element-type read-element-type)
                      (%tail-mask access-tail-mask)
                      (%tail access-tail))
          structure)
         (tail %tail)
         (tail-mask %tail-mask)
         (present (ldb-test (byte 1 offset) tail-mask)))
    (declare (type (or null cl-ds.common.rrb:node-content) tail)
             (type cl-ds.common.rrb:sparse-rrb-mask tail-mask)
             (type cl-ds.common.rrb:rrb-node-position offset)
             (type boolean present))
    (if present
        (bind ((old-bucket (aref tail offset))
               ((:values bucket status changed)
                (apply #'cl-ds.meta:grow-bucket! operation
                       container old-bucket value all)))
          (when changed
            (setf (aref tail offset) bucket))
          (values structure status))
        (bind (((:values bucket status changed)
                (apply #'cl-ds.meta:make-bucket
                       operation container
                       value all)))
          (when changed
            (let ((tail-array
                    (or tail
                        (make-array
                         cl-ds.common.rrb:+maximum-children-count+
                         :element-type element-type))))
              (setf (aref tail-array offset) bucket
                    %tail-mask (dpb 1 (byte 1 offset) tail-mask))
              (unless (eq tail tail-array)
                (setf %tail tail-array))))
          (values structure status)))))


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
              (cl-ds.common.rrb:with-sparse-rrb-node node
                (if all-exist
                    (bind ((current (cl-ds.common.rrb:sparse-nref node i))
                           ((:values new-bucket status changed)
                            (apply #'cl-ds.meta:grow-bucket! operation
                                   container current all)))
                      (when changed
                        (setf (cl-ds.common.rrb:sparse-nref node i) new-bucket))
                      (values structure status))
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
                     (insert-tail! structure nil)
                     (adjust-tree-to-new-size! structure position)
                     (apply #'set-in-tail!
                            structure operation container
                            position value all)
                     )))))))


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
             (for byte-position
                  from (* cl-ds.common.rrb:+bit-count+
                          shift)
                  downto 0
                  by cl-ds.common.rrb:+bit-count+)
             (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
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
