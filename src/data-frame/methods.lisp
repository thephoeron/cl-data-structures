(in-package #:cl-data-structures.data-frame)


(defun ensure-dimensionality (object more)
  (nest
   (unless (= #1=(cl-ds:dimensionality object) #2=(length more)))
   (error 'cl-ds:dimensionality-error :text)
   (format nil
           "Passed ~a arguments but data-frame dimensionality is ~a."
           #2# #1#)))


(defun ensure-in-frame (object more)
  (iterate
    (for m in more)
    (unless (fixnump m)
      (error 'cl:type-error :datum m
                            :expected-type 'non-negative-fixnum)))
  (when (some (curry #'> 0) more)
    (error 'cl-ds:argument-out-of-bounds
           :bounds "Must be non negative."
           :argument 'location
           :value more
           :text "Part of location is negative."))
  (unless (every #'< more #1=(read-upper-bounds object))
    (error 'cl-ds:argument-out-of-bounds
           :bounds #1#
           :value more
           :argument 'location
           :text "No such position in the data frame.")))


(defmethod cl-ds:at ((object data-frame) location &rest more)
  (let ((more (cons location more)))
    (ensure-dimensionality object more)
    (ensure-in-frame object more)
    (apply-aliases (read-aliases object) more)
    (at-data (access-data object)
             more)))


(defmethod (setf cl-ds:at) (new-value (object data-frame) location &rest more)
  (let ((more (cons location more)))
    (ensure-dimensionality object more)
    (ensure-in-frame object more)
    (apply-aliases (read-aliases object) more)
    (set-at-data new-value
                 (access-data object)
                 more)))


(-> mutable-data (cl-ds.seqs.rrb:mutable-rrb-vector non-negative-fixnum)
    cl-ds.seqs.rrb:transactional-rrb-vector)
(defun transactional-data (data count)
  (nest
   (lret ((result (cl-ds:become-transactional data))))
   (unless (eql 1 count))
   (iterate
     (with count = (1- count))
     (for i from 0 below (cl-ds:size result))
     (setf (cl-ds:at result i)
           (transactional-data (cl-ds:at result i)
                               count)))))


(-> mutable-data (cl-ds.seqs.rrb:transactional-rrb-vector non-negative-fixnum)
    cl-ds.seqs.rrb:mutable-rrb-vector)
(defun mutable-data (data count)
  (nest
   (lret ((result (cl-ds:become-mutable data))))
   (unless (eql 1 count))
   (iterate
     (with count = (1- count))
     (for i from 0 below (cl-ds:size result))
     (setf (cl-ds:at result i)
           (mutable-data (cl-ds:at result i)
                         count)))))


(defmethod mutate! ((data data-frame) dimension function &rest ranges)
  (bind ((old-instance (access-data data))
         (new-instance (transactional-data old-instance
                                           (cl-ds:dimensionality data)))
         (*active-data* (make-data-accessor new-instance data dimension)))
    (block outer
      (iterate
        (for i
             from (~> data read-lower-bounds (aref dimension))
             below (~> data read-upper-bounds (aref dimension)))
        (for extra-data =
             (iterate
               (for range in ranges)
               (for (values value more) = (cl-ds:consume-front range))
               (unless more
                 (return-from outer))
               (collect value)))
        (setf (access-position *active-data*) i)
        (apply function extra-data)))
    (setf (access-data data)
          (mutable-data new-instance
                        (cl-ds:dimensionality data)))
    data))


(defmethod cl-ds:size ((container data-frame))
  (iterate
    (for l in-vector (read-upper-bounds container))
    (for u in-vector (read-lower-bounds container))
    (multiplying (- u l))))


(defmethod set-alias ((container data-frame)
                      dimension
                      (name symbol)
                      position)
  (check-type dimension fixnum)
  (check-type position fixnum)
  (unless (<= 0 dimension (1- (cl-ds:dimensionality container)))
    (error 'cl-ds:dimensionality-error
           :text (format nil "No dimension ~a in data frame."
                         dimension)))
  (unless (<= #1=(~> container read-lower-bounds (aref dimension))
              position
              #2=(~> container read-upper-bounds (aref dimension) 1-))
    (error 'cl-ds:argument-out-of-bounds
           :text (format nil "No position ~a in dimension ~a"
                         position
                         dimension)
           :argument 'position
           :value position
           :bounds `(<= ,#1# #2#)))
  (let* ((key (cons dimension name))
         (reverse-aliases (read-reverse-aliases container))
         (result #3=(gethash key (read-aliases container))))
    (setf (gethash (cons dimension position) reverse-aliases) name
          #3# position)
    result))
