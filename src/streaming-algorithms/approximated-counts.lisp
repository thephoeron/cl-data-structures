(in-package #:cl-data-structures.streaming-algorithms)


(defclass approximated-counts ()
  ((%counters :initarg :counters
              :type vector
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


(defmethod cl-ds:at ((container approximated-counts)
                     location &rest more-locations)
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
    (with space = (read-space container))
    (for j from 0 below (read-count container))
    (minimize (aref counts (hashval hashes space j hash))
              into min)
    (finally (return (values min t)))))


(cl-ds.alg.meta:define-aggregation-function
    approximated-counts approximated-counts-function

  (:range hash-fn space count &key key hashes)
  (:range hash-fn space count &key (key #'identity) hashes)

  (%space %count %hash-fn %total %counters %hashes)

  ((&key hash-fn count space hashes)
   (check-type space positive-fixnum)
   (check-type count positive-fixnum)
   (check-type hashes (or null (simple-array fixnum (*))))
   (ensure-functionf hash-fn)
   (setf %hash-fn hash-fn
         %count count
         %space space
         %total 0
         %counters (make-array %space :initial-element 0
                                      :element-type 'non-negative-fixnum)
         %hashes (or hashes (make-hash-array count)))
   (unless (eql count (array-dimension %hashes 0))
     (error 'cl-ds:invalid-argument
            :argument 'hashes
            :format-control "Invalid first dimension of %hashes")))

  ((element)
   (incf %total)
   (iterate
     (with hash = (funcall %hash-fn element))
     (for j from 0 below %count)
     (for hashval = (hashval %hashes %space j hash))
     (incf (aref %counters hashval))))

  ((make 'approximated-counts
         :hash-fn %hash-fn
         :counters %counters
         :space %space
         :count %count
         :size %total
         :hashes %hashes)))
