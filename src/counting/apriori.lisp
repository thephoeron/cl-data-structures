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
              (return (sort (delete-if (lambda (x &aux (length (read-count x)))
                                         (or (< length minimal-support)
                                             (< (/ length total-size)
                                                minimal-frequency)))
                                       result)
                            #'< :key #'read-type)))))
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


(defun combine-nodes (node)
  (let* ((last-elt node)
         (parent (read-parent last-elt))
         (content (read-sets parent))
         (lower-bound (lower-bound content
                                   (read-type last-elt)
                                   #'<
                                   :key #'read-type)))
    (iterate
      (for i from (1+ lower-bound) below (length content))
      (for elt = (aref content i))
      (collect elt at start))))


(-> number-of-children (apriori-node) integer)
(defun number-of-children (node)
  (~> node read-sets length))


(defun children-at (node i)
  (~> node read-sets (aref i)))


(defun push-children (parent children)
  (vector-push-extend children (read-sets parent)))


(defun expand-node (index parent i queue)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type apriori-index index)
           (type apriori-node parent)
           (type fixnum i)
           (type lparallel.queue:queue queue))
  (iterate
    (while (< i (the fixnum (number-of-children parent))))
    (unless (eql (1+ i) (the fixnum (number-of-children parent)))
      (async-expand-node index parent (1+ i) queue))
    (let* ((node (children-at parent i))
           (supersets (combine-nodes node)))
      (declare (type list supersets)
               (type apriori-node node))
      (iterate
        (for superset in supersets)
        (for intersection = (the (vector fixnum) (ordered-intersection
                                                  #'< #'eql
                                                  (read-locations node)
                                                  (read-locations superset))))
        (for intersection-size = (length intersection))
        (when (or (< intersection-size (the fixnum (read-minimal-support index)))
                  (< (/ intersection-size (the fixnum (read-count node)))
                     (the single-float (read-minimal-frequency index))))
          (next-iteration))
        (for new-node = (make 'apriori-node
                              :locations intersection
                              :parent node
                              :type (read-type superset)))
        (push-children node new-node))
      (sort-sets node)
      (setf parent node i 0))))


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
  (cons (coerce (/ (read-count node) (read-total-size index)) 'single-float)
        (iterate
          (for n initially node then (read-parent n))
          (until (null (read-parent n)))
          (collect (node-name index n) at start))))


(flet ((add-to-stack (stack node)
         (reduce (flip #'cons)
                 (read-sets node)
                 :initial-value stack)))
  (defmethod cl-ds:whole-range ((object apriori-index))
    (cl-ds:xpr (:stack (list (read-root object)))
      (let ((node (pop stack)))
        (unless (null node)
          (if (null (read-parent node))
              (recur :stack (add-to-stack stack node))
              (send-recur (build-path object node)
                          :stack (add-to-stack stack node))))))))
