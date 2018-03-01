(in-package #:cl-data-structures.algorithms)


(defclass sequence-window (cl-ds:fundamental-random-access-range)
  ((%from :initarg :from
          :reader read-from)
   (%to :initarg :to
        :reader read-to)
   (%content :initarg :content
             :reader read-content)
   (%current-index :initarg :current-index
                   :reader read-current-index
                   :initform 0)))


(defclass random-access-sequence-window (sequence-window)
  ())


(defclass vector-sequence-window (sequence-window)
  ())


(defclass list-sequence-window (sequence-window)
  ())


(defgeneric sequence-window (sequence from to))


(defmethod sequence-window ((sequence cl-ds:fundamental-random-access-range)
                            from to)
  (unless (< from to)
    (error 'cl-ds:argument-out-of-bounds
           :argument 'from
           :bounds (list 0 to)
           :value from
           :text "FROM argument is out of bounds."))
  (unless (<= to (cl-ds:size sequence))
    (error 'cl-ds:argument-out-of-bounds
           :argument 'to
           :bounds (list from (cl-ds:size sequence))
           :value to
           :text "TO argument is out of bounds."))
  (make 'random-access-sequence-window
        :from from :to to
        :content sequence))


(defmethod sequence-window :before ((sequence sequence)
                                    from to)
  (unless (< from to)
    (error 'cl-ds:argument-out-of-bounds
           :argument 'from
           :bounds (list 0 to)
           :value from
           :text "FROM argument is out of bounds."))
  (unless (<= to (length sequence))
    (error 'cl-ds:argument-out-of-bounds
           :argument 'to
           :bounds (list from (length sequence))
           :value to
           :text "TO argument is out of bounds.")))


(defmethod sequence-window ((sequence sequence-window)
                            from to)
  (make (type-of sequence)
        :from (+ from (read-from sequence))
        :to (- (read-to sequence) to)
        :content (read-content sequence)))


(defmethod sequence-window ((sequence vector)
                            from to)
  (make 'vector-sequence-window
        :from from :to to
        :content sequence))


(defmethod sequence-window ((sequence list)
                            from to)
  (make 'list-sequence-window
        :from from :to to
        :content vector))


(defmethod cl-ds:at ((container vector-sequence-window) index)
  (bind (((:slots %from %to %content) container)
         (effective-index (+ %from index)))
    (when (< effective-index (length %content))
      (error 'cl-ds:argument-out-of-bounds
             :argument 'index
             :bounds (list 0 (- %to %from))
             :value index
             :text "Index out of range."))
    (values (aref %content effective-index) t)))


(defmethod cl-ds:at ((container list-sequence-window) index)
  (bind (((:slots %from %to %content) container)
         (effective-index (+ %from index)))
    (when (< effective-index (length %content))
      (error 'cl-ds:argument-out-of-bounds
             :argument 'index
             :bounds (list 0 (- %to %from))
             :value index
             :text "Index out of range."))
    (values (elt %content effective-index) t)))


(defmethod cl-ds:at ((container random-access-sequence-window) index)
  (bind (((:slots %from %to %content) container)
         (effective-index (+ %from index)))
    (when (< effective-index (cl-ds:size %content))
      (error 'cl-ds:argument-out-of-bounds
             :argument 'index
             :bounds (list 0 (- %to %from))
             :value index
             :text "Index out of range."))
    (cl-ds:at %content effective-index)))
