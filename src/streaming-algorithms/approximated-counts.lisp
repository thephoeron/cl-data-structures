(in-package #:cl-data-structures.streaming-algorithms)


(defclass approximated-counts (fundamental-data-sketch)
  ((%counters :initarg :counters
              :type vector
              :accessor access-counters)
   (%hashes :initarg :hashes
            :type vector
            :accessor access-hashes)
   (%count :initarg :count
           :type fixnum
           :accessor access-count)
   (%space :initarg :space
           :type fixnum
           :accessor access-space)))


(defmethod union ((first approximated-counts) &rest more)
  (cl-ds.utils:quasi-clone* first
    :counters (apply #'cl-ds.utils:transform
                     #'max
                     (~> first access-counters copy-array)
                     (mapcar #'access-counters more))))


(defmethod compatible-p ((first approximated-counts) &rest more)
  (push first more)
  (and (cl-ds.utils:homogenousp more :key #'access-count)
       (cl-ds.utils:homogenousp more :key #'access-space)
       (cl-ds.utils:homogenousp more :key (compose #'access-counters
                                                   #'length))
       (cl-ds.utils:homogenousp more :key #'access-hashes
                                     :test #'vector=)))


(defmethod initialize-instance :after ((object approximated-counts)
                                       &rest all)
  (declare (ignore all))
  (check-type (access-space object) integer)
  (check-type (access-count object) integer)
  (check-type (access-hashes object) (simple-array fixnum (* 2)))
  (check-type (access-counters object) (simple-array fixnum (*)))
  (unless (eql (array-dimension (access-counters object) 0)
               (access-space object))
    (error 'cl-ds:incompatible-arguments
           :parameters '(hashes space)
           :values `(,(access-hashes object) ,(access-space object))
           :format-control "First dimension of the COUNTERS is expected to be equal to SPACE."))
  (unless (eql (array-dimension (access-hashes object) 0)
               (access-count object))
    (error 'cl-ds:incompatible-arguments
           :parameters '(hashes space)
           :values `(,(access-hashes object) ,(access-space object))
           :format-control "First dimension of the hashes is expected to be equal to space.")))


(defmethod cl-ds.utils:cloning-information append
  ((sketch approximated-counts))
  '((:counters access-counters)
    (:hashes access-hashes)
    (:count access-count)))


(defmethod cl-ds:clone ((sketch approximated-counts))
  (cl-ds.utils:quasi-clone* sketch
    :counters (~> sketch access-counters copy-array)
    :hashes (~> sketch access-hashes)))


(defmethod cl-ds:at ((container approximated-counts)
                     location &rest more-locations)
  (unless (endp more-locations)
    (error 'cl-ds:dimensionality-error :bounds '(1)
                                       :value (1+ (length more-locations))
                                       :format-control "Approximated-counts does not accept more-locations"))
  (iterate
    (with hash = (ldb (byte 32 0)
                      (funcall (access-hash-fn container)
                               location)))
    (with counts = (access-counters container))
    (with hashes = (access-hashes container))
    (with space = (access-space container))
    (for j from 0 below (access-count container))
    (minimize (aref counts (hashval hashes space j hash))
              into min)
    (finally (return (values min t)))))


(cl-ds.alg.meta:define-aggregation-function
    approximated-counts approximated-counts-function

  (:range &key hash-fn space count key hashes data-sketch)
  (:range &key hash-fn space count (key #'identity) hashes
          (data-sketch (clean-sketch
                        #'approximated-counts
                        :hashes hashes
                        :hash-fn hash-fn
                        :space space
                        :count count)))

  (%data-sketch)

  ((&key data-sketch &allow-other-keys)
   (check-type data-sketch approximated-counts)
   (setf %data-sketch data-sketch))

  ((element)
   (incf (access-size %data-sketch))
   (iterate
     (with hash = (funcall (access-hash-fn %data-sketch) element))
     (with hashes = (access-hashes %data-sketch))
     (with counters = (access-counters %data-sketch))
     (with count = (access-count %data-sketch))
     (with space = (access-space %data-sketch))
     (for j from 0 below count)
     (for hashval = (hashval hashes space j hash))
     (incf (aref counters hashval))))

  (%data-sketch))


(defmethod clean-sketch ((function approximated-counts-function)
                         &rest all &key hashes hash-fn space count)
  (declare (ignore all))
  (check-type count integer)
  (check-type space integer)
  (check-type hashes (or null (simple-array fixnum (* 2))))
  (cl-ds:check-argument-bounds count (< 0 count))
  (cl-ds:check-argument-bounds space (< 0 space))
  (make 'approximated-counts
        :counters (make-array
                   space
                   :initial-element 0
                   :element-type 'non-negative-fixnum)
        :hashes (or hashes (make-hash-array count))
        :hash-fn (ensure-function hash-fn)
        :space space
        :count count))
