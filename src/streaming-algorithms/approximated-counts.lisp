(in-package #:cl-data-structures.streaming-algorithms)


(defclass approximated-counts (fundamental-data-sketch)
  ((%counters :initarg :counters
              :type vector
              :reader read-counters)
   (%hashes :initarg :hashes
            :type vector
            :accessor access-hashes)
   (%count :initarg :count
           :type fixnum
           :accessor access-count)
   (%space :initarg :space
           :type fixnum
           :accessor access-space)
   (%size :initarg :size
          :initform 0
          :type integer
          :accessor access-size
          :reader cl-ds:size)
   (%hash-fn :initarg :hash-fn
             :accessor access-hash-fn
             :type function)))


(defmethod initialize-instance :after ((object approximated-counts)
                                       &rest all)
  (declare (ignore all))
  (ensure-functionf (access-hash-fn object))
  (check-type (access-space object) positive-fixnum)
  (check-type (access-count object) positive-fixnum)
  (check-type (access-hashes object) (simple-array fixnum (*))))


(defmethod cl-ds.utils:cloning-information append
  ((sketch approximated-counts))
  '((:counters read-counters)
    (:hashes access-hashes)
    (:count access-count)
    (:size cl-ds:size)
    (:hash-fn access-hash-fn)))


(defmethod cl-ds:clone ((sketch approximated-counts))
  (cl-ds.utils:quasi-clone* sketch
    :counters (~> sketch read-counters copy-array)
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
    (with counts = (read-counters container))
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
          (data-sketch (make 'cl-ds.utils:cloning-information
                             :counters (make-array
                                        space
                                        :initial-element 0
                                        :element-type 'non-negative-fixnum)
                             :hashes (or hashes (make-hash-array count))
                             :hash-fn (ensure-function hash-fn)
                             :space space
                             :count count)))

  (%data-sketch)

  ((&key data-sketch)
   (setf %data-sketch data-sketch))

  ((element)
   (incf (access-size %data-sketch))
   (iterate
     (with hash = (funcall (access-hash-fn %data-sketch) element))
     (with hashes = (access-hashes %data-sketch))
     (with counters = (read-counters %data-sketch))
     (with count = (access-count %data-sketch))
     (with space = (access-space %data-sketch))
     (for j from 0 below count)
     (for hashval = (hashval hashes space j hash))
     (incf (aref counters hashval))))

  (%data-sketch))
