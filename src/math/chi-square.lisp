(in-package #:cl-data-structures.math)


(defclass chi-square-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric chi-square (range cell-value fields &key key)
  (:generic-function-class chi-square-function)
  (:method (range cell-value fields &key (key #'identity))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'chi-square
                                               :cell-value cell-value
                                               :fields fields
                                               :key key)))


(cl-ds:define-validation-for-fields
    (chi-square-function (:name :key))
  (:name :optional nil
         :type 'symbol)
  (:key :optional t
        :type 'function
        :default #'identity)
  (:test :optional t
         :type 'function
         :default #'eql)
  (:classes :optional t
            :default nil
            :type 'sequence))


(defun path-list (fields field-mapping &aux (result (vect)))
  (labels ((impl (path &optional (i 0))
             (if (eql i (length field-mapping))
                 (let ((rpath (reverse path)))
                   (vector-push-extend rpath result))
                 (iterate
                   (for class in-sequence (cl-ds:at (aref fields i) :classes))
                   (impl (cons class path) (1+ i))))))
    (impl nil)
    result))


(defun sum-plane (counts axis position)
  (bind ((address (make-list (array-dimension counts) :initial-element 0))
         (array-dimension (array-dimensions counts))
         (current address)
         (size (reduce #'* array-dimensions :initial-value 1))
         (pinned-size (/ size (elt address axis)))
         (pinned (nthcdr axis address))
         ((:dflet cycle-address ())
          (iterate
            (for cell on current)
            (for dim in array-dimension)
            (when (eq cell pinned)
              (next-iteration))
            (for val = (car cell))
            (when (= val dim)
              (iterate
                (for sub on address)
                (until (eq cell sub))
                ()))
            (when (< val dim)
              (incf (car cell))
              (setf current cell)
              (leave t))
            (finally ))))))


(defun chi-square-independent-counts (counts)
  (lret ((total-count (iterate ))
         (result (copy-array counts)))
    (iterate
      (for dim in (array-dimensions counts))
      (iterate
        (for i from 0 below )))))


(defun chi-square-on-table (fields &key class-counts &rest-all)
  (let ((total-sizes (chi-square-independent-counts class-counts)))
    ))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function chi-square-function)
     &rest all
     &key key fields cell-value)
  (let* ((all-fields (cons total-field fields))
         (field-counts (mapcar (compose #'length (rcurry #'cl-ds:at :classes)) fields))
         (field-mapping (map 'vector
                             (lambda (x)
                               (make-hash-table :test (cl-ds:at x :test)
                                                :size (~> x (cl-ds:at :classes) length)))
                             fields))
         (full-path-vector (path-list fields field-mapping)))
    (iterate
      (for f in fields)
      (for mapping in field-mapping)
      (iterate
        (for c in-sequence (cl-ds:at f :classes))
        (for i from 0)
        (setf (gethash c field-mapping) i)))
    (let ((full-address-vector (map 'vector
                                    (curry #'mapcar (flip #'gethash) field-mapping)
                                    full-path-vector)))
      (list (cl-ds.alg.meta:reduce-stage :class-counts
                (make-array field-counts :element-type 'fixnum
                                         :initial-element 0)
                (state element &rest all)
              (iterate
                (for path in-vector full-path-vector)
                (for address in-vector full-address-vector)
                (for old-value = (apply #'aref state address))
                (apply #'(setf aref)
                       (+ old-value (apply cell-value elt path))
                       state
                       address))
              state)

            (curry #'chi-square-on-table fields)))))
