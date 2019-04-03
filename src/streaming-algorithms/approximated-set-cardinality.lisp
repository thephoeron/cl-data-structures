(in-package #:cl-data-structures.streaming-algorithms)


(defclass approximated-set-cardinality (fundamental-data-sketch)
  ((%bits :initarg :bits
          :accessor access-bits)
   (%registers :initarg :registers
               :accessor access-registers)))


(defmethod cl-ds.utils:cloning-information append
    ((sketch approximated-set-cardinality))
  '((:bits access-bits)
    (:registers access-registers)))


(defmethod cl-ds:clone ((object approximated-set-cardinality))
  (cl-ds.utils:quasi-clone* object
    :registers (~> object access-registers copy-array)))


(defmethod compatible-p ((first-sketch fundamental-data-sketch)
                         &rest more-sketches)
  (push first-sketch more-sketches)
  (and (cl-ds.utils:homogenousp more-sketches
                                :key #'access-bits)
       (cl-ds.utils:homogenousp more-sketches
                                :key (compose #'access-registers #'length))))


(defmethod union ((first approximated-set-cardinality) &rest more)
  (cl-ds.utils:quasi-clone* first
    :registers (apply #'cl-ds.utils:transform #'max
                      (~> first access-registers copy-array)
                      (mapcar #'access-registers more))))


(defmethod initialize-instance :after ((object approximated-set-cardinality)
                                       &rest all)
  (declare (ignore all))
  (bind (((:slots %bits %registers %hash-fn) object))
    (check-type %bits integer)
    (unless (< 3 %bits 21)
      (error 'cl-ds:argument-out-of-bounds
             :argument 'bits
             :bounds (list 4 21)
             :value %bits
             :format-control "Bits out of range."))
    (check-type %registers (simple-array (unsigned-byte 8) (*)))
    (assert (eql (ash 1 %bits) (length %registers)))))


(defmethod cl-ds:value ((state approximated-set-cardinality))
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

  (:range &key bits hash-fn key data-sketch)
  (:range &key bits hash-fn (key #'identity)
          (data-sketch
           (clean-sketch #'approximated-set-cardinality
                         :bits bits
                         :hash-fn hash-fn)))

  (%data-sketch)

  ((&key data-sketch &allow-other-keys)

   (check-type data-sketch approximated-set-cardinality)
   (setf %data-sketch data-sketch))

  ((element)
   (bind (((:slots %hash-fn %bits %registers) %data-sketch)
          (hash (ldb (byte 32 0) (funcall %hash-fn element)))
          (index (ash hash (- (- 32 %bits))))
          (hash-length (integer-length hash))
          (rank (if (zerop hash-length) 0 (1- hash-length))))
     (when (> rank (aref %registers index))
       (setf (aref %registers index) rank))))

  (%data-sketch))


(defmethod clean-sketch ((function approximated-set-cardinality-function)
                         &rest all &key bits hash-fn)
  (declare (ignore all))
  (make 'approximated-set-cardinality
        :bits bits
        :registers (make-array (ash 1 bits)
                               :element-type '(unsigned-byte 8))
        :hash-fn hash-fn))
