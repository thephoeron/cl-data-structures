(in-package #:cl-data-structures.streaming-algorithms)


(defclass bloom-filter ()
  ((%counters :initarg :counters
              :type simple-bit-vector
              :reader read-counters)
   (%hashes :initarg :hashes
            :type vector
            :reader read-hashes)
   (%depth :initarg :depth
           :type fixnum
           :reader read-depth)
   (%width :initarg :width
           :type fixnum
           :reader read-width)
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
                                       :text "Approximated-counts does not accept more-locations"))
  (iterate
    (with hash = (funcall (read-hash-fn container) location))
    (with counts = (read-counters container))
    (with hashes = (read-hashes container))
    (with width = (read-width container))
    (for j from 0 below (read-depth container))
    (for value = (aref counts j (hashval hashes width j hash)))
    (when (zerop value)
      (leave (values nil t)))
    (finally (return (values t t)))))


(cl-ds.alg.meta:define-aggregation-function
    bloom-filter bloom-filter-function

  (:range hash-fn depth width &key key hashes)
  (:range hash-fn depth width &key (key #'identity) hashes)

  (%width %depth %hash-fn %total %counters %hashes)

  ((&key hash-fn depth width hashes)
   (check-type depth positive-fixnum)
   (check-type width positive-fixnum)
   (check-type hashes (or null (simple-array fixnum (* 2))))
   (ensure-functionf hash-fn)
   (setf %hash-fn hash-fn
         %width width
         %depth depth
         %total 0
         %counters (make-array (list %depth %width) :element-type 'bit)
         %hashes (or hashes (make-hash-array depth)))
   (unless (eql %depth (array-dimension %hashes 0))
     (error 'cl-ds:invalid-argument
            :argument 'hashes
            :text "Invalid first dimension of %hashes")))

  ((element)
   (incf %total)
   (iterate
     (with hash = (funcall %hash-fn element))
     (for j from 0 below %depth)
     (for hashval = (hashval %hashes %width j hash))
     (setf (aref %counters j hashval) 1)))

  ((make 'bloom-filter
         :hash-fn %hash-fn
         :counters %counters
         :depth %depth
         :width %width
         :size %total
         :hashes %hashes)))
