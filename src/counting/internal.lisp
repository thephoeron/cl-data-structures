(in-package #:cl-data-structures.counting)


(-> sort-sets (set-index-node) set-index-node)
(defun sort-sets (node)
  (write-sets (sort (read-sets node) #'< :key #'read-type)
              node)
  node)


(defun make-set-index (table total-size minimal-support)
  (let* ((root-content
           (iterate
             (with result = (make-array (hash-table-count table)))
             (for i from 0)
             (for (key value) in-hashtable table)
             (for (id . positions) = value)
             (setf (aref result i) (make 'set-index-node
                                         :type id
                                         :locations positions))
             (finally
              (return (sort (delete-if (rcurry #'< minimal-support)
                                       result
                                       :key #'read-count)
                            #'<
                            :key #'read-type)))))
         (root (make-instance 'set-index-node
                              :sets root-content
                              :count total-size))
         (result (make 'set-index
                       :total-size total-size
                       :minimal-support minimal-support
                       :root root)))
    (map nil (curry #'write-parent root) root-content)
    result))


(-> combine-nodes (set-index-node) list)
(defun combine-nodes (node)
  (let* ((last-elt node)
         (parent (read-parent last-elt))
         (content (read-sets parent))
         (lower-bound (lower-bound (the vector content)
                                   (the fixnum (read-type last-elt))
                                   #'<
                                   :key #'read-type)))
    (iterate
      (for i from (1+ lower-bound) below (length content))
      (for elt = (aref content i))
      (collect elt at start))))


(-> expand-node
    (set-index set-index-node non-negative-fixnum lparallel.queue:queue)
    t)
(-> async-expand-node
    (set-index set-index-node non-negative-fixnum lparallel.queue:queue)
    t)
(with-compilation-unit nil
  (defun expand-node (index parent i queue)
    (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
    (iterate
      (with minimal-support = (the fixnum (read-minimal-support index)))
      (for children-count = (the fixnum (number-of-children parent)))
      (while (< i children-count))
      (unless (eql (1+ i) children-count)
        (async-expand-node index parent (1+ i) queue))
      (for node = (child-at parent i))
      (for supersets = (combine-nodes node))
      (for count = (the fixnum (read-count node)))
      (for locations = (the (vector fixnum) (read-locations node)))
      (iterate
        (for superset in supersets)
        (for intersection = (the (vector fixnum) (ordered-intersection
                                                  #'< #'eql
                                                  locations
                                                  (read-locations superset))))
        (for intersection-size = (length intersection))
        (when (< intersection-size minimal-support)
          (next-iteration))
        (for new-node = (make 'set-index-node
                              :locations intersection
                              :type (read-type superset)))
        (push-child node new-node))
      (sort-sets node)
      (setf parent node i 0)))


  (defun async-expand-node (index parent i queue)
    (~> (expand-node index parent i queue)
        lparallel:future
        (lparallel.queue:push-queue queue))))


(-> number-of-children (set-index-node) integer)
(defun number-of-children (node)
  (~> node read-sets length))


(-> child-at (set-index-node non-negative-fixnum) set-index-node)
(defun child-at (node i)
  (~> node read-sets (aref i)))


(defun push-child (parent child)
  (vector-push-extend child (read-sets parent))
  (write-parent parent child))


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


(defun add-to-stack (stack node depth)
  (iterate
    (for elt in-vector (read-sets node))
    (push (list* elt depth) stack))
  stack)


(defun child-of-type (parent type)
  (let* ((content (read-sets parent))
         (lower-bound (lower-bound content
                                   type
                                   #'<
                                   :key #'read-type)))
    (when (and (< lower-bound (length content))
               (eql (read-type (aref content lower-bound))
                    type))
      (aref content lower-bound))))


(defun node-at-type (index &rest more-types)
  (iterate
    (with path = (sort more-types #'<))
    (with node = (read-root index))
    (while path)
    (setf node (child-of-type node (first path))
          path (rest path))
    (while node)
    (finally (return node))))


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
           :text "Duplicated values in the content of sets.")))


(defun node-at-names (index name &rest more-names)
  (let* ((names (cons name more-names))
         (path (mapcar (curry #'name-to-type index) names)))
    (when (every #'identity path)
      (validate-unique-names names)
      (apply #'node-at-type index path))))


(defun entropy-from-node (parent)
  (iterate
    (with content = (read-sets parent))
    (for node in-vector content)
    (for frequency = (frequency node))
    (sum (* frequency (log frequency 2))
         into result)
    (finally (return (- result)))))


(defun data-range (index minimal-frequency
                   &optional (operation #'identity) maximal-size)
  (cl-ds:xpr (:stack (list (list* (read-root index) 0)))
    (bind ((cell (pop stack)))
      (unless (null cell)
        (bind (((node . depth) cell))
          (if (and maximal-size (< maximal-size depth))
              (recur :stack stack)
              (if (null (read-parent node))
                  (recur :stack (add-to-stack stack node
                                              (1+ depth)))
                  (if (< (/ (support node) (read-total-size index))
                         minimal-frequency)
                      (recur :stack (add-to-stack stack node
                                                  (1+ depth)))
                      (send-recur (funcall operation node)
                                  :stack (add-to-stack stack node
                                                       (1+ depth)))))))))))


(defun chain-node (node)
  (iterate
    (for n initially node then (read-parent n))
    (while (read-type n))
    (collect n at start)))


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


(defun just-post (apriori aposteriori)
  (when (and apriori aposteriori)
    (cl-ds.utils:ordered-exclusion
     (lambda (a b) (< (read-type a) (read-type b)))
     (lambda (a b) (eql (read-type a) (read-type b)))
     (~> aposteriori chain-node (coerce 'vector))
     (~> apriori chain-node (coerce 'vector)))))


(defun pure-aposteriori (set)
  (when-let ((types (~> (just-post (read-apriori-node set) (read-node set))
                        (map 'list #'read-type _))))
    (apply #'node-at-type (read-index set) types)))
