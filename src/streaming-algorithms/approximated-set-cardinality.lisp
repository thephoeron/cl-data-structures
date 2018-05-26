(in-package #:cl-data-structures.streaming-algorithms)


(defclass approximated-set-cardinality-result ()
  ((%bits :initarg :bits)
   (%registers :initarg :registers)))


(defmethod cl-ds:value ((state approximated-set-cardinality-result))
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
                             (log (- 1.0 (/ estimate
                                            4294967296.0)))))))
    estimate))


(cl-ds.alg.meta:define-aggregation-function
    approximated-set-cardinality approximated-set-cardinality-function

  (:range bits hash-fn &key key)
  (:range bits hash-fn &key (key #'identity))

  (%bits %registers %hash-fn)

  ((&key bits hash-fn &allow-other-keys)
   (unless (< 3 bits 21)
     (error 'cl-ds:argument-out-of-bounds
            :argument 'bits
            :bounds (list 4 21)
            :value bits
            :text "Bits out of range."))
   (setf %bits bits
         %hash-fn hash-fn
         %registers (make-array (ash 1 bits)
                                :element-type 'unsigned-byte)))

  ((element)
   (bind ((hash (ldb (byte 32 0) (funcall %hash-fn element)))
          (index (ash hash (- (- 32 %bits))))
          (hash-length (integer-length hash))
          (rank (if (zerop hash-length) 0 (1- hash-length))))
     (when (> rank (aref %registers index))
       (setf (aref %registers index) rank))))

  ((make 'approximated-set-cardinality-result
         :bits %bits
         :registers %registers)))
