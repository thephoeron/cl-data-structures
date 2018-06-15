(in-package cl-data-structures.counting)


(defclass apriori-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric apriori (range minimal-support minimal-frequency &key key parallel)
  (:generic-function-class apriori-function)
  (:method (range minimal-support minimal-frequency &key (key #'identity) (parallel nil))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'apriori
                                               :parallel parallel
                                               :minimal-support
                                               :minimal-frequency
                                               :key key)))


(defclass apriori-node ()
  ((%type :reader read-type
          :initarg :type
          :type integer)
   (%locations :reader read-locations
               :initarg :locations
               :type (vector fixnum))
   (%sets :reader read-sets
          :writer write-sets
          :type vector
          :initarg :sets
          :initform (vect))))


(defun read-count (node)
  (length (read-locations node)))


(defun sort-sets (node)
  (write-sets (sort (read-sets node) #'< :key #'read-type)
              node)
  node)


(defclass apriori-index ()
  ((%root :reader read-root
          :initarg :root)
   (%minimal-support :reader read-minimal-support
                     :initarg :minimal-support)
   (%minimal-frequency :reader read-minimal-frequency
                       :initarg :minimal-frequency)
   (%total-size :reader read-total-size
                :initarg :total-size)))


(defun make-apriori-index (vector-data minimal-support minimal-frequency)
  (let ((table (make-hash-table)))
    (iterate
      (for i from 0)
      (for set in-vector vector-data)
      (iterate
        (for elt in-vector set)
        (ensure #1=(gethash elt table) (make-array 32 :element-type 'fixnum
                                                      :adjustable t
                                                      :fill-pointer 0))
        (vector-push-extend i #1#)))
    (let ((root-content (make-array (hash-table-count table))))
      (iterate
        (for i from 0)
        (for (key value) in-hashtable table)
        (for (number count positions) = value)
        (setf (aref root-content i) (make 'apriori-node
                                          :type number
                                          :count count
                                          :locations positions)))
      (let ((total-size (length vector-data)))
        (make 'apriori-index
              :total-size total-size
              :minimal-support minimal-support
              :minimal-frequency minimal-frequency
              :root (~> root-content
                        (delete-if (lambda (x &aux (length (read-count x)))
                                     (or (< length minimal-support)
                                         (< (/ length total-size) minimal-frequency)))
                                   _)
                        (sort #'< :key #'read-type)))))))


(defun construct-supersets (index existing-path)
  (let ((root (read-root index)))
    (if (null existing-path)
        (iterate
          (with size = (length root))
          (for i from 1 below size)
          (for elt = (aref root i))
          (iterate
            (for j from 0 below i)
            (for elt2 = (aref root j))
            (unless (eql (read-type elt) (read-type elt2))
              (collect (lret ((array (make-array 2 :element-type 'fixnum)))
                         (setf (aref array 1) elt
                               (aref array 0) elt2))
                at start))))
        (let* ((existing-set (map-into (make-array (1+ (length existing-path)))
                                       #'identity existing-path))
               (length (length existing-set))
               (larger nil))
          (assert (ordered-p existing-set #'< :key #'read-type))
          (iterate
            (for elt in-vector root)
            (for key = (the fixnum (read-type elt)))
            (when (or larger (eql (lower-bound existing-set
                                               key #'<
                                               :key #'read-type)
                                  length))
              (setf larger t)
              (setf (last-elt existing-set) elt)))
          (if larger
              (list existing-set)
              nil)))))


(define-constant +empty-sets+ (list nil) :test 'equal)


(defun insert-one-into-index (index subset)
  (let* ((prototype-node (last-elt subset))
         (last-existing-node (aref subset (- (length subset) 2)))
         (sets (read-sets last-existing-node))
         (intersecting-indexes (apply #'ordered-intersection
                                      #'< #'eql
                                      prototype-node
                                      last-existing-node)))
    (if (< (length intersecting-indexes)
           (read-minimal-support index))
        nil
        (progn
          (assert (null (find (read-type prototype-node)
                              sets
                              :test #'eql
                              :key #'read-type)))
          (let ((new-node (make 'apriori-node
                                :locations intersecting-indexes
                                :type (read-type prototype-node))))
            (vector-push-extend new-node
                                sets)
            (sort-sets last-existing-node)
            (let ((result (copy-array subset)))
              (setf (last-elt result) new-node)
              (list result)))))))


(defun insert-into-index (index all-subsets)
  (lret ((result nil))
    (map nil
         (compose (curry #'map nil (lambda (x) (push x result)))
                  (curry #'insert-one-into-index index))
         all-subsets)))


(defun apriori-algorithm (minimal-support minimal-frequency &key set-form)
  (bind (((vector . table) set-form)
         (index (make-apriori-index vector
                                    minimal-support
                                    minimal-frequency)))
    (iterate
      (with sets = +empty-sets+)
      (map-into sets (curry #'construct-supersets index) sets)
      (while sets)
      (for effective-new-sets = (insert-into-index index sets))
      (setf sets effective-new-sets))))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function apriori-function)
     &rest all &key parallel minimal-support minimal-frequency key &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :set-form (range &rest all)
          (declare (ignore all))
          (let ((table (make-hash-table :test 'equal))
                (vector (vect)))
            (iterate
              (with i = 0)
              (for j from 0)
              (for (data more) = (cl-ds:consume-front range))
              (while more)
              (for k = (funcall key data))
              (cl-ds:across (lambda (k) (ensure (gethash k table) (finc i)))
                            k)
              (vector-push-extend (~> k
                                      (cl-ds.alg:to-vector
                                       :key (rcurry #'gethash table))
                                      (sort #'<))
                                  vector))
            (cons vector table)))
        (curry #'apriori-algorithm minimal-support minimal-frequency)))
