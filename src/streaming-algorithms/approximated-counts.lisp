(in-package #:cl-data-structures.streaming-algorithms)


(define-constant +long-prime+ 4294967311)


(defclass approximated-counts ()
  ((%counters :initarg :counters
              :type vector
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
   (%hash-fn :initarg :hash-fn
             :reader read-hash-fn
             :type function)))


(defun hashval (hashes width j hash)
  (~> (aref hashes j 0)
      (* hash)
      (+ (aref hashes j 1))
      (rem +long-prime+)
      (rem width)))


(defmethod cl-ds:at ((container approximated-counts) location &rest more-locations)
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
    (minimize (aref counts j (hashval hashes width j hash))
              into min)
    (finally (return (values min t)))))


(defun make-min-counting-hash-array (depth)
  (lret ((result (make-array (list depth 2) :element-type 'fixnum)))
    (map-into (cl-ds.utils:unfold-table result)
              (lambda () (truncate (1+ (/ (* (random most-positive-fixnum)
                                        +long-prime+)
                                     (1- most-positive-fixnum))))))))


(cl-ds.alg.meta:define-aggregation-function
    approximated-counts approximated-counts-function

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
         %counters (make-array (list %depth %width) :initial-element 0)
         %hashes (or hashes (make-min-counting-hash-array depth)))
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
     (incf (aref %counters j hashval))))

  ((make 'approximated-counts
         :hash-fn %hash-fn
         :counters %counters
         :depth %depth
         :width %width
         :hashes %hashes)))
