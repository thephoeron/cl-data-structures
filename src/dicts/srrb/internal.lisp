(in-package #:cl-data-structures.dicts.srrb)


(-> insert-tail-handle-root-overflow
    (fixnum
     cl-ds.common.rrb:sparse-rrb-node
     cl-ds.common.rrb:sparse-rrb-node
     t)
    cl-ds.common.rrb:sparse-rrb-node)
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


(-> make-node-from-tail
    (fundamental-sparse-rrb-vector
     t)
    cl-ds.common.rrb:sparse-rrb-node)
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


(-> insert-tail! (mutable-sparse-rrb-vector)
    mutable-sparse-rrb-vector)
(defun insert-tail! (structure)
  (declare (optimize (debug 3)))
  (let ((tail-mask (access-tail-mask structure))
        (ownership-tag nil))
    (unless (zerop tail-mask)
      (bind ((new-node (make-node-from-tail structure ownership-tag))
             ((:accessors (tree access-tree)
                          (tree-size access-tree-size)
                          (%shift access-shift)
                          (tree-index-bound access-tree-index-bound))
              structure)
             (root tree)
             (shift %shift))
        (declare (type non-negative-fixnum shift))
        (cond ((null root)
               (setf tree new-node))
              ((>= (ash tree-index-bound (- cl-ds.common.rrb:+bit-count+))
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
                   (for index = (ldb (byte cl-ds.common.rrb:+bit-count+
                                           position)
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
                     (let* ((new-element
                              (cl-ds.common.rrb:make-sparse-rrb-node
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
    (when (eql tail-mask (lognot cl-ds.common.rrb:+tail-mask+))
      (setf (access-tail structure) nil))
    (incf (access-index-bound structure)
          cl-ds.common.rrb:+maximum-children-count+)
    (incf (access-tree-size structure) (logcount tail-mask)))
  structure)


(-> transactional-insert-tail! (transactional-sparse-rrb-vector t)
    transactional-sparse-rrb-vector)
(defun transactional-insert-tail! (structure ownership-tag)
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
        (declare (type non-negative-fixnum shift))
        (cond ((null root)
               (setf tree new-node))
              ((>= (ash tree-index-bound (- cl-ds.common.rrb:+bit-count+))
                   (ash 1 (* cl-ds.common.rrb:+bit-count+ shift))) ; overflow
               (let ((new-root (insert-tail-handle-root-overflow
                                shift root new-node ownership-tag)))
                 (incf %shift)
                 (setf tree new-root)))
              (t (bind ((position (* cl-ds.common.rrb:+bit-count+ shift))
                        (size (access-tree-index-bound structure))
                        ((:labels impl (node byte-position depth))
                         (decf depth)
                         (if (zerop depth)
                             new-node
                             (let* ((index (ldb (byte cl-ds.common.rrb:+bit-count+
                                                      position)
                                                size))
                                    (present (and node (cl-ds.common.rrb:sparse-nref
                                                        node index)))
                                    (next-node (and present (cl-ds.common.rrb:sparse-nref
                                                             node index)))
                                    (current-node (or node
                                                      (cl-ds.common.rrb:make-rrb-node
                                                       :content (make-array 1)
                                                       :ownership-tag ownership-tag)))
                                    (owned (cl-ds.common.abstract:acquire-ownership current-node
                                                                                    ownership-tag))
                                    (new-node (impl next-node
                                                    (- byte-position
                                                       cl-ds.common.rrb:+bit-count+)
                                                    depth)))
                               (if owned
                                   (progn
                                     (setf (cl-ds.common.rrb:sparse-nref current-node index)
                                           new-node)
                                     current-node)
                                   (let ((copy (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                                current-node (if present 0 1)
                                                ownership-tag)))
                                     (setf (cl-ds.common.rrb:sparse-nref copy index) new-node)
                                     copy)))))
                        (new-tree (impl root (* cl-ds.common.rrb:+bit-count+ shift) shift)))
                   (unless (eq new-tree root)
                     (setf tree new-tree)))))))
    (setf (access-tail-mask structure) 0
          (access-tree-index-bound structure) (access-index-bound structure))
    (when (eql tail-mask (lognot cl-ds.common.rrb:+tail-mask+))
      (setf (access-tail structure) nil))
    (incf (access-index-bound structure)
          cl-ds.common.rrb:+maximum-children-count+)
    (incf (access-tree-size structure) (logcount tail-mask))))
  structure)


(-> make-adjusted-tree (fundamental-sparse-rrb-vector fixnum t)
    cl-ds.common.rrb:sparse-rrb-node)
(defun make-adjusted-tree (structure position ownership-tag)
  (declare (optimize (debug 3)))
  (bind (((:accessors (root access-tree)
                      (tree-size access-tree-size)
                      (shift access-shift)
                      (tree-index-bound access-tree-index-bound))
          structure)
         (old-shift shift)
         (old-tree-index-bound tree-index-bound)
         (new-tree-index-bound
          (* cl-ds.common.rrb:+maximum-children-count+
             (ceiling position cl-ds.common.rrb:+maximum-children-count+)))
         (new-shift (~> new-tree-index-bound
                        1-
                        integer-length
                        (/ cl-ds.common.rrb:+bit-count+)
                        ceiling
                        1-))
         (shift-difference (- new-shift old-shift))
         (larger? (> shift-difference 0)))
    (declare (type non-negative-fixnum new-shift
                   new-tree-index-bound
                   old-tree-index-bound
                   old-shift)
             (type boolean larger?)
             (type fixnum shift-difference))
    (assert (not (zerop shift-difference)))
    (if larger?
        (iterate
          (with highest-current = (1- old-tree-index-bound))
          (with new-root = (cl-ds.common.rrb:make-sparse-rrb-node
                            :content (make-array 1)
                            :ownership-tag ownership-tag))
          (with node = new-root)
          (repeat (1- shift-difference))
          (for byte-position
               from (* cl-ds.common.rrb:+bit-count+
                       new-shift)
               downto 0
               by cl-ds.common.rrb:+bit-count+)
          (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                        highest-current))
          (for present =
               (cl-ds.common.rrb:sparse-rrb-node-contains node i))
          (unless present
            (insert-new-node! node i))
          (setf node (cl-ds.common.rrb:sparse-nref node i))
          (finally
           (let ((i (ldb (byte cl-ds.common.rrb:+bit-count+
                               (* (1+ old-shift)
                                  cl-ds.common.rrb:+bit-count+))
                         highest-current)))
             (cl-ds.common.rrb:with-sparse-rrb-node node
               (setf (cl-ds.common.rrb:sparse-nref node i) root))
             (return (values new-root new-shift new-tree-index-bound)))))
        (iterate
          (for byte-position
               from (* cl-ds.common.rrb:+bit-count+
                       old-shift)
               downto 0
               by cl-ds.common.rrb:+bit-count+)
          (repeat (- shift-difference))
          (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                        (1- position)))
          (for node initially root
               then (cl-ds.common.rrb:sparse-nref node i))
          (finally (return (values node new-shift new-tree-index-bound)))))))


(-> adjust-tree-to-new-size! (mutable-sparse-rrb-vector fixnum t)
    mutable-sparse-rrb-vector)
(defun adjust-tree-to-new-size! (structure position ownership-tag)
  (bind (((:values new-root new-shift new-bound)
          (make-adjusted-tree structure position ownership-tag)))
    (setf (access-shift structure) new-shift
          (access-tree-index-bound structure) new-bound
          (access-tree structure) new-root)
    structure))


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


(-> insert-new-node! (cl-ds.common.rrb:sparse-rrb-node
                      cl-ds.common.rrb:rrb-node-position)
    cl-ds.common.rrb:sparse-rrb-node)
(defun insert-new-node! (node i)
  (let* ((old-content (cl-ds.common.rrb:sparse-rrb-node-content node))
         (old-content-size (array-dimension old-content 0))
         (old-bitmask (cl-ds.common.rrb:sparse-rrb-node-bitmask node))
         (new-bitmask (dpb 1 (byte 1 i) old-bitmask))
         (splice-index (logcount (ldb (byte i 0) new-bitmask)))
         (old-count (logcount old-bitmask))
         (new-count (1+ old-count))
         (new-content (if (<= new-count old-content-size)
                          old-content
                          (make-array
                           new-count
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
           :content (make-array 1
                                :element-type (array-element-type
                                               old-content)))

          (cl-ds.common.rrb:sparse-rrb-node-bitmask node) new-bitmask)
    node))


(-> transactional-grow-tree! (cl-ds.meta:grow-function
                              transactional-sparse-rrb-vector
                              transactional-sparse-rrb-vector
                              fixnum
                              list
                              t)
    transactional-sparse-rrb-vector)
(defun transactional-grow-tree! (operation structure container position all value)
  (bind ((final-status nil)
         (ownership-tag (cl-ds.common.abstract:read-ownership-tag structure))
         (operation-type (type-of operation))
         ((:labels impl (node byte-position depth))
          (let* ((i (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                         position))
                 (present (and node (cl-ds.common.rrb:sparse-rrb-node-contains node i))))
            (when (and (not present)
                       (member operation-type
                               '(cl-ds.meta:update!-function
                                 cl-ds.meta:update-if!-function)))
              (return-from transactional-grow-tree!
                (values structure
                        cl-ds.common:empty-eager-modification-operation-status)))
            (if (zerop depth)
                (if present
                    (bind ((current (cl-ds.common.rrb:sparse-nref node i))
                           ((:values new-bucket status changed)
                            (apply #'cl-ds.meta:grow-bucket operation
                                   container current all)))
                      (if changed
                          (let ((owned (cl-ds.common.abstract:acquire-ownership
                                        node ownership-tag)))
                            (if owned
                                (progn
                                  (setf (cl-ds.common.rrb:sparse-nref node i) new-bucket)
                                  (return-from transactional-grow-tree!
                                    (values structure status)))
                                (setf node (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                            node 0 ownership-tag)
                                      final-status status
                                      (cl-ds.common.rrb:sparse-nref node i) new-bucket))
                            node)
                          (return-from transactional-grow-tree!
                            (values structure status))))
                    (bind (((:values new-bucket status changed)
                            (apply #'cl-ds.meta:make-bucket
                                   operation container
                                   value all))
                           (node (or node (cl-ds.common.rrb:make-sparse-rrb-node
                                           :content (make-array
                                                     1
                                                     :element-type (read-element-type structure))
                                           :ownership-tag ownership-tag)))
                           (owned (cl-ds.common.abstract:acquire-ownership node ownership-tag)))
                      (if changed
                          (if owned
                              (progn
                                (setf (cl-ds.common.rrb:sparse-nref node i) new-bucket)
                                (return-from transactional-grow-tree!
                                  (values structure status)))
                              (progn
                                (setf node (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                            node 0 ownership-tag)
                                      final-status status
                                      (cl-ds.common.rrb:sparse-nref node i) new-bucket)
                                node))
                          (return-from transactional-grow-tree!
                            (values structure status)))))
                (if present
                    (let* ((next-node (cl-ds.common.rrb:sparse-nref node i))
                           (new-node (impl next-node
                                           (- byte-position cl-ds.common.rrb:+bit-count+)
                                           (1- depth))))
                      (if (eq new-node next-node)
                          (return-from transactional-grow-tree!
                            (values structure
                                    final-status))
                          (progn
                            (unless (cl-ds.common.abstract:acquire-ownership
                                     node ownership-tag)
                              (setf node (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                          node 0 ownership-tag)))
                            (setf (cl-ds.common.rrb:sparse-nref node i) new-node)
                            node)))
                    (let ((new-node (impl nil
                                          (- byte-position cl-ds.common.rrb:+bit-count+)
                                          (1- depth)))
                          (current-node (or node (cl-ds.common.rrb:make-sparse-rrb-node
                                                  :content (make-array 1)))))
                      (setf (cl-ds.common.rrb:sparse-nref current-node i) new-node)
                      current-node)))))
         (shift (access-shift structure))
         (tree (access-tree structure))
         (new-tree (impl tree
                         (* cl-ds.common.rrb:+bit-count+ shift)
                         shift)))
    (unless (eq tree new-tree)
      (setf (access-tree structure) new-tree))
    (values structure final-status)))


(-> destructive-grow-tree! (cl-ds.meta:grow-function
                            mutable-sparse-rrb-vector
                            mutable-sparse-rrb-vector
                            fixnum
                            list
                            t)
    mutable-sparse-rrb-vector)
(defun destructive-grow-tree! (operation structure container position all value)
  (bind ((final-status nil)
         (operation-type (type-of operation))
         ((:labels impl (node byte-position depth))
          (let* ((i (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                         position))
                 (present (and node (cl-ds.common.rrb:sparse-rrb-node-contains node i))))
            (when (and (not present)
                       (member operation-type
                               '(cl-ds.meta:update!-function
                                 cl-ds.meta:update-if!-function)))
              (return-from destructive-grow-tree!
                (values structure
                        cl-ds.common:empty-eager-modification-operation-status)))
            (if (zerop depth)
                (if present
                    (bind ((current (cl-ds.common.rrb:sparse-nref node i))
                           ((:values new-bucket status changed)
                            (apply #'cl-ds.meta:grow-bucket! operation
                                   container current all)))
                      (if changed
                          (progn
                            (setf (cl-ds.common.rrb:sparse-nref node i) new-bucket)
                            (return-from destructive-grow-tree!
                              (values structure status)))
                          (return-from destructive-grow-tree!
                            (values structure status))))
                    (bind (((:values new-bucket status changed)
                            (apply #'cl-ds.meta:make-bucket
                                   operation container
                                   value all))
                           (node (or node (cl-ds.common.rrb:make-sparse-rrb-node
                                           :content (make-array
                                                     1
                                                     :element-type (read-element-type structure))))))
                      (if changed
                          (progn
                            (setf (cl-ds.common.rrb:sparse-nref node i) new-bucket
                                  final-status status)
                            (incf (access-tree-size structure))
                            node)
                          (return-from destructive-grow-tree!
                            (values structure
                                    status)))))
                (if present
                    (let* ((next-node (cl-ds.common.rrb:sparse-nref node i))
                           (new-node (impl next-node
                                           (- byte-position cl-ds.common.rrb:+bit-count+)
                                           (1- depth))))
                      (if (eq new-node next-node)
                          (return-from destructive-grow-tree!
                            (values structure
                                    final-status))
                          (setf (cl-ds.common.rrb:sparse-nref node i) new-node))
                      node)
                    (let ((new-node (impl nil
                                          (- byte-position cl-ds.common.rrb:+bit-count+)
                                          (1- depth)))
                          (current-node (or node (cl-ds.common.rrb:make-sparse-rrb-node
                                                  :content (make-array 1)))))
                      (setf (cl-ds.common.rrb:sparse-nref current-node i) new-node)
                      current-node)))))
         (shift (access-shift structure)))
    (impl (access-tree structure)
          (* cl-ds.common.rrb:+bit-count+ shift)
          shift)
    (values structure final-status)))

