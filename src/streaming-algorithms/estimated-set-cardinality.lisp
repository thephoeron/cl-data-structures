(in-package #:cl-data-structures.streaming-algorithms)


(defclass estimated-set-cardinality-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric estimated-set-cardinality (range bits hash-fn &key key)
  (:generic-function-class estimated-set-cardinality-function)
  (:method (range bits hash-fn &key key)
    (cl-ds.alg:apply-aggregation-function range
                                          #'estimated-set-cardinality
                                          :key key
                                          :hash-fn hash-fn
                                          :bits bits)))


(defclass estimated-set-cardinality-state ()
  ((%bits :initarg :bits)
   (%registers :initarg :registers)
   (%hash-fn :initarg :hash-fn)))


(defmethod cl-ds.alg:aggregate ((function estimated-set-cardinality-function)
                                state
                                element)
  (bind (((:slots %bits %registers %hash-fn) state)
         (hash (funcall %hash-fn element)) ;ensure that there is at least one 1 bit
         (index (ash hash (- (- 32 %bits))))
         (rank (integer-length (ldb (byte 32 0) hash))))
    (when (> rank (aref %registers index))
      (setf (aref %registers index) rank))))


(defmethod cl-ds.alg:state-result ((function estimated-set-cardinality-function)
                                   state)
  state)


(defmethod cl-ds.alg:make-state ((function estimated-set-cardinality-function)
                                 &key bits hash-fn &allow-other-keys)
  (unless (< 3 bits 21)
    (error 'cl-ds:argument-out-of-bounds
           :argument 'bits
           :bounds (list 4 21)
           :value bits
           :text "Bits out of range."))
  (make 'estimated-set-cardinality-state
        :bits bits
        :hash-fn hash-fn
        :registers (make-array (ash 1 bits) :element-type 'unsigned-byte)))


(defmethod cl-ds:value ((state estimated-set-cardinality-state))
  (bind (((:slots %bits %registers) state)
         (size (length %registers))
         (alpha-mm (* (cond ((eql 4 %bits) 0.673)
                            ((eql 5 %bits) 0.697)
                            ((eql 6 %bits) 0.709)
                            (t (/ 0.7213 (+ 1.0 (/ 1.079 size)))))
                      (expt size 2)))
         (sum (iterate
                (for r in-vector %registers)
                (sum (/ 1.0 (ash 1 r)))))
         (estimate (/ alpha-mm sum)))
    (cond ((<= estimate (* size (/ 5.0 2.0)))
           (iterate
             (for r in-vector %registers)
             (counting (zerop r) into result)
             (finally (unless (zerop result)
                        (setf estimate (* size (log (/ size result))))))))
          ((> estimate (* (/ 1.0 30.0) 4294967296.0))
           (setf estimate (* -4294967296.0
                             (log (- 1.0 (/ estimate 4294967296.0)))))))
    estimate))
