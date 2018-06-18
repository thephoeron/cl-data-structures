(in-package cl-data-structures.counting)


(defclass apriori-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric apriori (range minimal-support minimal-frequency
                     &key key)
  (:generic-function-class apriori-function)
  (:method (range minimal-support minimal-frequency
            &key (key #'identity))
    (ensure-functionf key)
    (check-type minimal-support positive-fixnum)
    (check-type minimal-frequency single-float)
    (cl-ds.alg.meta:apply-aggregation-function
     range
     #'apriori
     :minimal-support minimal-support
     :minimal-frequency minimal-frequency
     :key key)))


(defclass apriori-node ()
  ((%type :reader read-type
          :initarg :type
          :initform nil
          :type (or null integer))
   (%locations :reader read-locations
               :initarg :locations
               :type (vector fixnum))
   (%count :reader read-count
           :initarg :count
           :type integer)
   (%sets :reader read-sets
          :writer write-sets
          :type vector
          :initarg :sets
          :initform (vect))
   (%parent :initarg :parent
            :initform nil
            :writer write-parent
            :reader read-parent)))


(defmethod initialize-instance :after ((node apriori-node)
                                       &key &allow-other-keys)
  (when (slot-boundp node '%locations)
    (setf (slot-value node '%count)
          (length (read-locations node)))))


(defun sort-sets (node)
  (write-sets (sort (read-sets node) #'< :key #'read-type)
              node)
  node)


(defclass apriori-index ()
  ((%root :reader read-root
          :initarg :root)
   (%minimal-support :reader read-minimal-support
                     :initarg :minimal-support)
   (%reverse-mapping :accessor access-reverse-mapping
                     :initform nil)
   (%mapping :accessor access-mapping
             :initform nil)
   (%minimal-frequency :reader read-minimal-frequency
                       :initarg :minimal-frequency)
   (%total-size :reader read-total-size
                :initarg :total-size)))


(defun frequency (node)
  (if-let ((parent (read-parent node)))
    (coerce (/ (read-count node) (read-count parent)) 'single-float)
    1.0))


(defun make-apriori-index (table total-size minimal-support minimal-frequency)
  (let* ((root-content
           (iterate
             (with result = (make-array (hash-table-count table)))
             (for i from 0)
             (for (key value) in-hashtable table)
             (for (id . positions) = value)
             (setf (aref result i) (make 'apriori-node
                                         :type id
                                         :locations positions))
             (finally
              (return (sort (delete-if (rcurry #'< minimal-support)
                                       result
                                       :key #'read-count)
                            #'<
                            :key #'read-type)))))
         (root (make-instance 'apriori-node
                              :sets root-content
                              :count total-size))
         (result (make 'apriori-index
                       :total-size total-size
                       :minimal-support minimal-support
                       :minimal-frequency minimal-frequency
                       :root root)))
    (map nil (curry #'write-parent root) root-content)
    result))


(-> combine-nodes (apriori-node) list)
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


(-> number-of-children (apriori-node) integer)
(defun number-of-children (node)
  (~> node read-sets length))


(defun child-at (node i)
  (~> node read-sets (aref i)))


(defun push-child (parent child)
  (vector-push-extend child (read-sets parent))
  (write-parent parent child))


(defun expand-node (index parent i queue)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type apriori-index index)
           (type apriori-node parent)
           (type fixnum i)
           (type lparallel.queue:queue queue))
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
      (for new-node = (make 'apriori-node
                            :locations intersection
                            :type (read-type superset)))
      (push-child node new-node))
    (sort-sets node)
    (setf parent node i 0)))


(defun async-expand-node (index parent i queue)
  (~> (expand-node index parent i queue)
      lparallel:future
      (lparallel.queue:push-queue queue)))


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


(defun add-to-stack (stack node)
  (reduce (flip #'cons)
          (read-sets node)
          :initial-value stack))


(defun build-path-node (node)
  (iterate
    (with size = 0)
    (for n initially node then (read-parent n))
    (while (read-parent n))
    (incf size)
    (finally (iterate
               (with result = (make-array size))
               (for n initially node then (read-parent n))
               (for j from (1- size) downto 0)
               (while n)
               (setf (aref result j) n)
               (finally (return-from build-path-node result))))))


(defun make-path-generator (node)
  (cl-ds:xpr (:stack (list node))
    (let ((node (pop stack)))
      (unless (null node)
        (if (null (read-parent node))
            (recur :stack (add-to-stack stack node))
            (if (emptyp (read-sets node))
                (send-recur (build-path-node node)
                            :stack stack)
                (recur :stack (add-to-stack stack node))))))))


(defun shuffle-nodes (path)
  (iterate
    (with source = (list path))
    (with result = nil)
    (for i from 0 below (length path))
    (iterate
      (for s in source)
      (iterate
        (for j from (1+ i) below (length path))
        (for next = (copy-array s))
        (rotatef (aref next i) (aref next j))
        (push next result)))
    (setf source result)
    (finally (return result))))


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


(defun intersection-from-index (index path)
  (iterate
    (with root = (read-root index))
    (for i from 1 below (length path))
    (for type = (read-type (aref path i)))
    (collect (read-locations (child-of-type root type)) at start into result)
    (finally (return (apply #'ordered-intersection #'< #'eql result)))))


(defun insert-shuffled-node (index root path)
  (iterate
    (for i from 0 below (length path))
    (for node = (aref path i))
    (setf (aref path i) root)
    (when (eq node (read-root index))
      (next-iteration))
    (for type = (read-type node))
    (for child = (child-of-type root type))
    (if (null child)
        (let ((new (make-instance
                    'apriori-node
                    :type type
                    :locations (intersection-from-index index path))))
          (push-child root new)
          (setf root new))
        (setf root child))))


(defun add-shuffled-nodes (index)
  (let ((new-paths (~> (read-root index)
                       make-path-generator
                       (cl-ds.alg:on-each #'shuffle-nodes _)
                       cl-ds.alg:flatten-lists
                       cl-ds.alg:to-vector
                       (lparallel:psort #'path-compare)))
        (root (read-root index)))
    (iterate
      (for elt in-vector new-paths)
      (insert-shuffled-node index root elt)
      (lparallel:pmap nil #'sort-sets elt))))


(defun apriori-algorithm (&key set-form minimal-support
                            minimal-frequency &allow-other-keys)
  (bind (((_ total-size . table) set-form)
         (index (make-apriori-index table
                                    total-size
                                    minimal-support
                                    minimal-frequency))
         (queue (lparallel.queue:make-queue)))
    (async-expand-node index (read-root index) 0 queue)
    (let ((reverse-mapping (make-array (hash-table-count table)))
          (mapping (make-hash-table :size (hash-table-count table))))
      (iterate
        (for i from 0)
        (for (key value) in-hashtable table)
        (for (id . positions) = value)
        (setf (aref reverse-mapping id) key
              (gethash key mapping) i))
      (setf (access-mapping index) mapping
            (access-reverse-mapping index) reverse-mapping)
      (iterate
        (for (values f more) = (lparallel.queue:try-pop-queue queue))
        (while more)
        (lparallel:force f))
      (add-shuffled-nodes index)
      (reset-locations index)
      index)))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function apriori-function)
     &rest all
     &key minimal-support minimal-frequency key &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:reduce-stage :set-form
            (list* -1 0 (make-hash-table :test 'equal))
            (state data &rest all)
          (declare (ignore all))
          (bind (((_ position . table) state))
            (cl-ds:across (lambda (k)
                            (ensure (gethash k table)
                              (list* (incf (car state))
                                     (make-array 4 :element-type 'fixnum
                                                   :adjustable t
                                                   :fill-pointer 0)))
                            (vector-push-extend position
                                                (cdr (gethash k table))))
                          (funcall key data)))
          (incf (second state))
          state)
        #'apriori-algorithm))


(defun node-name (index node)
  (aref (access-reverse-mapping index)
        (read-type node)))


(defun build-path (index node)
  (cons (coerce (/ (read-count node) (read-count (read-parent node)))
                'single-float)
        (iterate
          (for n initially node then (read-parent n))
          (until (null (read-parent n)))
          (collect (node-name index n) at start))))


(defmethod cl-ds:whole-range ((object apriori-index))
  (cl-ds:xpr (:stack (list (read-root object)))
    (let ((node (pop stack)))
      (unless (null node)
        (if (null (read-parent node))
            (recur :stack (add-to-stack stack node))
            (if (< (frequency node)
                   (read-minimal-frequency object))
                (recur :stack (add-to-stack stack node))
                (send-recur (build-path object node)
                            :stack (add-to-stack stack node))))))))
