(in-package #:cl-data-structures.data-frame)


(defun ensure-dimensionality (object more)
  (nest
   (unless (= #1=(cl-ds:dimensionality object) #2=(length more)))
   (error
    'cl-ds:dimensionality-error
    :text (format nil
                  "Passed ~a arguments but data-frame dimensionality is ~a."
                  #2# #1#)
    :value #2#
    :bounds #1#)))


(defun fixnump (x)
  (typep x 'fixnum))


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
  (push location more)
  (at-data-frame object more))


(defmethod (setf cl-ds:at) (new-value (object data-frame) location &rest more)
  (push location more)
  (set-at-data-frame new-value object more))


(defmethod cl-ds:at ((object proxy-data-frame) location &rest more)
  (push location more)
  (ensure-dimensionality object more)
  (apply-aliases (read-aliases object) more)
  (setf more (insert-pinned-axis object more))
  (at-data-frame (read-inner-data-frame object) more))


(defmethod (setf cl-ds:at) (new-value (object proxy-data-frame)
                            location &rest more)
  (push location more)
  (ensure-dimensionality object more)
  (apply-aliases (read-aliases object) more)
  (setf more (insert-pinned-axis object more))
  (set-at-data-frame new-value
                     (read-inner-data-frame object)
                     (proxy-data-frame-effective-address object more)))


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
         (accessor (make-data-accessor new-instance data dimension)))
    (common-mutate! (~> data read-lower-bounds (aref dimension))
                    (~> data read-upper-bounds (aref dimension))
                    accessor function ranges)
    (setf (access-data data)
          (mutable-data new-instance
                        (cl-ds:dimensionality data)))
    data))


(defmethod mutate! ((data proxy-data-frame) dimension function &rest ranges)
  (bind ((old-instance (~> data read-inner-data-frame access-data))
         (new-instance (transactional-data old-instance
                                           (~> data
                                               read-inner-data-frame
                                               cl-ds:dimensionality)))
         (accessor (make-data-accessor new-instance data dimension)))
    (common-mutate! (~> data read-lower-bounds (aref dimension))
                    (~> data read-upper-bounds (aref dimension))
                    accessor function ranges)
    (setf (~> data read-inner-data-frame access-data)
          (mutable-data new-instance
                        (~> data
                            read-inner-data-frame
                            cl-ds:dimensionality)))))


(defmethod cl-ds:size ((container fundamental-data-frame))
  (iterate
    (for u in-vector (read-upper-bounds container))
    (for l in-vector (read-lower-bounds container))
    (multiplying (- u l))))


(defmethod (setf alias) ((name symbol)
                         (container fundamental-data-frame)
                         dimension
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
           :bounds `(<= ,#1# ,#2#)))
  (let* ((key (cons dimension name))
         (reverse-aliases (read-reverse-aliases container))
         (result #3=(gethash key (read-aliases container))))
    (setf (gethash (cons dimension position) reverse-aliases) name
          #3# position)
    result))


(defmethod alias ((container fundamental-data-frame) dimension position)
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
           :bounds `(<= ,#1# ,#2#)))
  (gethash (cons dimension position) (read-reverse-aliases container)))


(defmethod plane ((data data-frame) &rest more)
  (if (endp more)
      data
      (let ((length (length more)))
        (when (oddp length)
          (error 'cl-ds:invalid-argument
                 :text "&rest arguments should come in even number!"
                 :argument 'more))
        (when (>= (/ length 2) (cl-ds:dimensionality data))
          (error 'cl-ds:dimensionality-error
                 :text "Can't slice plane because number of axis passed must be lower then dimensionality of frame."))
        (iterate
          (with aliases = (read-aliases data))
          (for m on more)
          (for p-m previous m)
          (for k initially nil then (not k))
          (when k
            (check-type (first p-m) integer)
            (check-type (first m) (or symbol integer))
            (setf (first m) (apply-alias aliases (first p-m) (first m)))))
        (proxy-plane data (~> more (batches 2) (sort #'< :key #'car))))))


(defmethod at-cell ((object data-accessor)
                    location &rest more-locations)
  (let ((frame (read-frame object))
        (location (location-list (cons location more-locations)
                                 (read-dimension object)
                                 (access-position object))))
    (apply-aliases (read-aliases object) location)
    (ensure-dimensionality frame location)
    (ensure-in-frame frame location)
    (nth-value 0 (at-data (read-data object) location))))


(defmethod (setf at-cell) (new-value
                           (object data-accessor)
                           location &rest more-locations)
  (let ((frame (read-frame object))
        (location (location-list (cons location more-locations)
                                 (read-dimension object)
                                 (access-position object))))
    (apply-aliases (read-aliases object) location)
    (ensure-dimensionality frame location)
    (ensure-in-frame frame location)
    (set-at-data new-value (read-data object) location)
    (cl-ds:force new-value)))


(defmethod at-cell ((object proxy-data-accessor)
                    location &rest more-locations)
  (let ((frame (read-frame object))
        (location (location-list (cons location more-locations)
                                 (read-dimension object)
                                 (access-position object))))
    (apply-aliases (read-aliases frame) location)
    (ensure-dimensionality frame location)
    (ensure-in-frame frame location)
    (setf location (insert-pinned-axis frame location))
    (setf location (proxy-data-frame-effective-address object location))
    (apply-aliases (~> frame read-inner-data-frame read-aliases)
                   location)
    (at-data (read-data object) location)))


(defmethod (setf at-cell) (new-value
                           (object proxy-data-accessor)
                           location &rest more-locations)
  (let ((frame (read-frame object))
        (location (location-list (cons location more-locations)
                                 (read-dimension object)
                                 (access-position object))))
    (apply-aliases (read-aliases frame) location)
    (ensure-dimensionality frame location)
    (setf location (insert-pinned-axis frame location))
    (ensure-in-frame frame location)
    (setf location (proxy-data-frame-effective-address object location))
    (apply-aliases (~> frame read-inner-data-frame read-aliases)
                   location)
    (set-at-data new-value
                 (read-data object)
                 location)))
