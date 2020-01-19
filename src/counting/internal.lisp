(cl:in-package #:cl-data-structures.counting)


(-> sort-sets (set-index-node) set-index-node)
(defun sort-sets (node)
  (write-sets (sort (read-sets node) #'< :key #'read-type)
              node)
  node)


(defun make-set-index (table total-size minimal-support)
  (bind (((:values root-content children)
          (iterate
            (with result = (make-array (hash-table-count table)))
            (for i from 0)
            (for (key value) in-hashtable table)
            (for (id . positions) = value)
            (setf (aref result i) (list* (make 'set-index-node
                                               :type id
                                               :count (length positions))
                                         positions))
            (finally
             (let* ((effective (~> (delete-if (rcurry #'< minimal-support)
                                              result
                                              :key (compose #'read-count #'car))
                                   (sort #'< :key (compose #'read-type #'car))))
                    (children (map 'vector #'cdr effective)))
               (map-into effective #'car effective)
               (return (values effective children))))))
         (root (make-instance 'tree-set-index-node
                              :sets root-content
                              :count total-size))
         (result (make 'set-index
                       :total-size total-size
                       :minimal-support minimal-support
                       :root root)))
    (values result children)))


(-> sort-key (list) fixnum)
(defun sort-key (x)
  (read-type (car x)))


(-> combine-nodes (set-index-node set-index-node vector) list)
(defun combine-nodes (node parent children)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((last-elt node)
         (content (map 'vector #'list*
                       (the vector (read-sets parent))
                       children))
         (lower-bound (lower-bound (the vector content)
                                   (the fixnum (read-type last-elt))
                                   #'<
                                   :key #'sort-key)))
    (iterate
      (for i from (1+ lower-bound) below (length content))
      (for elt = (aref content i))
      (collect elt at start))))


(-> expand-node
    (set-index set-index-node simple-vector non-negative-fixnum
     lparallel.queue:queue)
    t)
(-> async-expand-node
    (set-index set-index-node simple-vector non-negative-fixnum
     lparallel.queue:queue)
    t)
(with-compilation-unit nil
  (defun expand-node (index parent children i queue)
    (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
             (type set-index-node parent)
             (type simple-vector children)
             (type non-negative-fixnum i))
    (iterate
      (with new-children = (vect))
      (with minimal-support = (read-minimal-support index))
      (declare (type vector new-children)
               (type fixnum minimal-support))
      (for children-count = (length children))
      (while (< i children-count))
      (unless (eql (the fixnum (1+ i)) children-count)
        (async-expand-node index parent children (1+ i) queue))
      (for node = (the set-index-node (child-at parent i)))
      (for supersets = (the list (combine-nodes node parent children)))
      (for locations = (the (vector fixnum) (aref children i)))
      (for count = (length locations))
      (iterate
        (declare (type (vector fixnum) node-locations))
        (for (node . node-locations) in supersets)
        (for intersection = (the (vector fixnum) (ordered-intersection
                                                  #'< #'eql
                                                  locations
                                                  node-locations)))
        (for intersection-size = (length intersection))
        (when (< intersection-size minimal-support)
          (next-iteration))
        (for new-node = (make 'set-index-node
                              :count intersection-size
                              :type (read-type node)))
        (vector-push-extend (list* new-node intersection)
                            new-children))
      (setf new-children
            (sort new-children #'<
                  :key #'sort-key))
      (map nil (lambda (x) (push-child node (car x)))
           new-children)
      (setf parent (the set-index-node node)
            i 0
            children (map 'vector #'cdr new-children)
            (fill-pointer new-children) 0)))


  (defun async-expand-node (index parent children i queue)
    (~> (expand-node index parent children i queue)
        lparallel:future
        (lparallel.queue:push-queue queue))))


(-> number-of-children (set-index-node) integer)
(defun number-of-children (node)
  (~> node read-sets length))


(-> child-at (set-index-node non-negative-fixnum) set-index-node)
(defun child-at (node i)
  (~> node read-sets (aref i)))


(defun push-child (parent child)
  (unless (typep parent 'tree-set-index-node)
    (setf parent (change-class parent 'tree-set-index-node)))
  (vector-push-extend child (read-sets parent)))


(defun reset-locations (index)
  (labels ((impl (node)
             (slot-makunbound node '%locations)
             (iterate
               (for n in-vector (read-sets node))
               (impl n))))
    (impl (read-root index))))


(defun path-compare (a b)
  (lexicographic-compare #'< #'eql a b
                         :key #'read-type))


(defun add-to-stack (stack cell depth)
  (iterate
    (with node = (car cell))
    (for elt in-vector (read-sets node))
    (push (list* elt depth cell) stack))
  stack)


(defun child-of-type (parent type)
  (let* ((content (read-sets parent))
         (lower-bound (lower-bound content
                                   type
                                   #'<
                                   :key #'read-type)))
    (when (and (< lower-bound (length content))
               (= (read-type (aref content lower-bound))
                  type))
      (aref content lower-bound))))


(defun node-at-type (index types)
  (declare (type sequence types)
           (optimize (speed 3)))
  (iterate
    (declare (type fixnum i))
    (with path = (~> types
                     (coerce '(vector fixnum))
                     (sort #'<)))
    (with root = (read-root index))
    (with node = root)
    (with result-path = (make-array (length path)))
    (for i from 0 below (length path))
    (setf node (child-of-type node (aref path i)))
    (until (null node))
    (setf (aref result-path i) node)
    (finally (return (values (unless (eq root node) node)
                             result-path)))))


(defun node-at (index i &rest more-i)
  (iterate
    (for elt in more-i)
    (for node
         initially (child-at (read-root index) i)
         then (child-at node elt))
    (while node)
    (finally (return node))))


(defun validate-unique-names (names)
  (unless (iterate
            (with table = (make-hash-table :test 'equal))
            (for name in names)
            (always (null #1=(gethash name table)))
            (setf #1# t))
    (error 'cl-ds:operation-not-allowed
           :format-control "Duplicated values in the content of sets."))
  names)


(defun node-at-names (index names)
  (let ((path (mapcar (curry #'name-to-type index) names)))
    (when (every #'identity path)
      (validate-unique-names names)
      (node-at-type index path))))


(defun data-range (index minimal-frequency
                   &optional (operation #'identity) maximal-size)
  (cl-ds:xpr (:stack (list (list* (read-root index) 0 nil)))
    (when (endp stack)
      (cl-ds:finish))
    (bind ((cell (first stack))
           ((node depth . parent) cell))
      (when (and maximal-size (< maximal-size depth))
          (cl-ds:recur :stack (rest stack)))
      (when (null parent)
        (cl-ds:recur :stack (add-to-stack (rest stack) cell (1+ depth))))
      (if (< (/ (support node) (read-total-size index))
             minimal-frequency)
          (cl-ds:recur :stack (add-to-stack stack cell (1+ depth)))
          (cl-ds:send-recur (funcall operation cell)
                            :stack (add-to-stack (rest stack)
                                                 cell
                                                 (1+ depth)))))))


(defun chain-cells (cell)
  (iterate
    (for (n depth . parent)
         initially cell
         then parent)
    (collect n at start)
    (until (endp parent))))


(defun set-name (set)
  (node-name (read-index set)
             (read-node set)))


(defun type-to-name (index type)
  (aref (access-reverse-mapping index)
        type))


(defun node-name (index node)
  (type-to-name index (read-type node)))


(defun name-to-type (index name)
  (gethash name (access-mapping index)))
