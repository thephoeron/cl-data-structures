(in-package #:cl-data-structures.streaming-algorithms)


(defclass bloom-filter ()
  ((%counters :initarg :counters
              :type simple-bit-vector
              :reader read-counters)
   (%hashes :initarg :hashes
            :type vector
            :reader read-hashes)
   (%count :initarg :count
           :type fixnum
           :reader read-count)
   (%space :initarg :space
           :type fixnum
           :reader read-space)
   (%size :initarg :size
          :type integer
          :reader cl-ds:size)
   (%hash-fn :initarg :hash-fn
             :reader read-hash-fn
             :type function)))


(defmethod cl-ds:at ((container bloom-filter) location &rest more-locations)
  (unless (endp more-locations)
    (error 'cl-ds:dimensionality-error :bounds '(1)
                                       :value (1+ (length more-locations))
                                       :format-control "Approximated-counts does not accept more-locations"))
  (iterate
    (with hash = (ldb (byte 32 0)
                      (funcall (read-hash-fn container)
                               location)))
    (with counts = (read-counters container))
    (with hashes = (read-hashes container))
    (with count = (read-count container))
    (with space = (read-space container))
    (for j from 0 below count)
    (for value = (aref counts (hashval hashes space j hash)))
    (when (zerop value)
      (leave (values nil t)))
    (finally (return (values t t)))))


(cl-ds.alg.meta:define-aggregation-function
    bloom-filter bloom-filter-function

  (:range hash-fn space count &key key hashes)
  (:range hash-fn space count &key (key #'identity) hashes)

  (%count %space %hash-fn %total %counters %hashes)

  ((&key hash-fn space count hashes)
   (check-type space positive-fixnum)
   (check-type count positive-fixnum)
   (check-type hashes (or null (simple-array fixnum (* 2))))
   (ensure-functionf hash-fn)
   (setf %hash-fn hash-fn
         %count count
         %space space
         %total 0
         %counters (make-array space :element-type 'bit)
         %hashes (or hashes (make-hash-array count))))

  ((element)
   (incf %total)
   (iterate
     (with hash = (funcall %hash-fn element))
     (for j from 0 below %count)
     (for position = (hashval %hashes %space j hash))
     (setf (aref %counters position) 1)))

  ((make 'bloom-filter
         :hash-fn %hash-fn
         :counters %counters
         :count %count
         :space %space
         :size %total
         :hashes %hashes)))
