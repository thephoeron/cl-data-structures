(in-package #:cl-data-structures.threads)


(defclass queue-range (cl-ds:fundamental-forward-range)
  ((%queue :initarg :queue
           :accessor access-queue)
   (%initial-queue :initarg :initial-queue
                   :initform nil
                   :accessor access-initial-queue)
   (%capacity :initarg :capacity
              :reader read-capacity
              :initform nil)
   (%lock :initform (bt:make-lock)
          :reader read-lock)
   (%cv :initform (bt:make-condition-variable)
        :reader read-cv)))


(defmethod cl-ds:clone ((range queue-range))
  (bt:with-lock-held ((read-lock range))
    (let* ((old-queue (access-queue range))
           (new-queue (cl-ds:become-transactional old-queue))
           (another-queue (cl-ds:become-transactional old-queue))
           (initial-queue (cl-ds:become-transactional old-queue)))
      (setf (access-queue range) new-queue)
      (make 'queue-range
            :queue another-queue
            :initial-queue initial-queue
            :capacity (read-capacity range)))))


(defmethod cl-ds:reset! ((range queue-range))
  (bt:with-lock-held ((read-lock range))
    (let* ((initial-queue (access-initial-queue range)))
      (setf (access-queue range) (if (null initial-queue)
                                     (~> range access-queue
                                         cl-ds:empty-clone)
                                     (cl-ds:clone initial-queue)))
      range)))


(defmethod cl-ds:consume-front ((range queue-range))
  (bt:with-lock-held ((read-lock range))
    (let ((capacity (read-capacity range))
          (cv (read-cv range))
          (lock (read-lock range)))
      (when (~> range access-queue null)
        (return-from cl-ds:consume-front (values nil nil)))
      (iterate
        (for queue = (access-queue range))
        (when (null queue)
          (return-from cl-ds:consume-front (values nil nil)))
        (while (~> queue cl-ds:size zerop))
        (bt:condition-wait cv lock))
      (bind (((type . data) (~> range access-queue cl-ds:take-out!)))
        (switch (type)
          (:end (setf (access-queue range) nil) (values nil nil))
          (:data (values data t)))))))


(defmethod cl-ds:peek-front ((range queue-range))
  (bt:with-lock-held ((read-lock range))
    (let ((capacity (read-capacity range))
          (cv (read-cv range))
          (lock (read-lock range)))
      (when (~> range access-queue null)
        (return-from cl-ds:peek-front (values nil nil)))
      (iterate
        (for queue = (access-queue range))
        (when (null queue)
          (return-from cl-ds:consume-front (values nil nil)))
        (while (~> queue cl-ds:size zerop))
        (bt:condition-wait cv lock))
      (bind (((type . data) (~> range access-queue (cl-ds:at :front))))
        (switch (type)
          (:end (values nil nil))
          (:data (values data t)))))))


(defun make-queue-range (&key capacity)
  (make 'queue-range
        :capacity capacity
        :queue (cl-ds.queues.2-3-tree:make-transactional-2-3-queue)))
