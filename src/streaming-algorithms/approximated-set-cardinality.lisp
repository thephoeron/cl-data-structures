(cl:in-package #:cl-data-structures.streaming-algorithms)


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
    :registers (~> object access-registers copy-hash-table)))


(defmethod compatible-p ((first-sketch fundamental-data-sketch)
                         &rest more-sketches)
  (push first-sketch more-sketches)
  (and (cl-ds.utils:homogenousp more-sketches
                                :key #'access-bits)
       (cl-ds.utils:homogenousp more-sketches
                                :key (compose #'access-registers #'length))))


(defmethod union ((first approximated-set-cardinality) &rest more)
  (let ((result-table (~> first access-registers copy-hash-table)))
    (iterate
      (for m in more)
      (iterate
        (for (key value) in-hashtable m)
        (maxf (gethash key result-table 0) value)))
    (cl-ds.utils:quasi-clone* first
      :registers result-table)))


(defmethod initialize-instance :after ((object approximated-set-cardinality)
                                       &rest all)
  (declare (ignore all))
  (bind (((:slots %bits %registers %hash-fn) object))
    (check-type %bits integer)
    (unless (<= 4 %bits 20)
      (error 'cl-ds:argument-out-of-bounds
             :argument 'bits
             :bounds (list 4 20)
             :value %bits
             :format-control "Bits out of range."))
    (check-type %registers hash-table)))


(defmethod cl-ds:value ((state approximated-set-cardinality))
  (bind (((:slots %bits %registers) state)
         (size (ash 1 %bits))
         (alpha-mm (* (cond ((eql 4 %bits) 0.673d0)
                            ((eql 5 %bits) 0.697d0)
                            ((eql 6 %bits) 0.709d0)
                            (t (/ 0.7213d0 (+ 1.0 (/ 1.079d0 size)))))
                      (expt size 2)))
         (hash-table-count (hash-table-count %registers))
         (zero-registers (- size hash-table-count))
         (sum (iterate
                (with s = zero-registers)
                (for (k r) in-hashtable %registers)
                (incf s (/ 1.0 (ash 1 r)))
                (finally (return s))))
         (estimate (/ alpha-mm sum)))
    (cond ((<= estimate (* size (/ 5.0 2.0)))
           (unless (zerop zero-registers)
             (setf estimate (* size (log (/ size zero-registers))))))
          ((> estimate (* (/ 1.0 30.0d0) 4294967296.0d0))
           (setf estimate (* -4294967296.0d0
                             (log (- 1.0 (/ estimate 4294967296.0d0)))))))
    estimate))


(declaim (inline hll-rank))
(defun hll-rank (hash bits)
  (declare (type fixnum hash bits)
           (optimize (speed 3)
                     (debug 0)
                     (safety 0)))
  (iterate
    (declare (type fixnum i j))
    (for i from 1)
    (for j from 32 downto bits)
    (unless (~> hash (logand 1) zerop)
      (finish))
    (setf hash (ash hash (- 1)))
    (finally (return i))))


(cl-ds.alg.meta:define-aggregation-function
    approximated-set-cardinality approximated-set-cardinality-function

  (:range &key bits hash-fn key data-sketch)
  (:range &key bits hash-fn (key #'identity)
          (data-sketch
           (clean-sketch #'approximated-set-cardinality
                         :bits bits
                         :hash-fn hash-fn)))

  (%data-sketch)

  ((check-type data-sketch approximated-set-cardinality)
   (setf %data-sketch (cl-ds:clone data-sketch)))

  ((element)
   (bind (((:slots %hash-fn %bits %registers) %data-sketch)
          (hash-fn %hash-fn)
          (registers %registers)
          (bits %bits)
          (hash (ldb (byte 32 0) (funcall hash-fn element)))
          (index (ash hash (- (the (unsigned-byte 8)
                                   (- 32 bits)))))
          (rank (hll-rank hash bits)))
     (declare (optimize (speed 3) (debug 0) (safety 1) (space 0))
              (type function hash-fn)
              (type fixnum hash)
              (type hash-table registers)
              (type fixnum bits index rank))
     (assert (<= 4 bits 20))
     (maxf (the fixnum (gethash index registers 0)) rank)))

  (%data-sketch))


(defmethod clean-sketch ((function approximated-set-cardinality-function)
                         &rest all &key bits hash-fn)
  (declare (ignore all))
  (ensure-functionf hash-fn)
  (check-type bits integer)
  (cl-ds:check-argument-bounds bits (<= 4 bits 20))
  (make 'approximated-set-cardinality
        :bits bits
        :registers (make-hash-table)
        :hash-fn hash-fn))
