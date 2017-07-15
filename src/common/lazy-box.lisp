(in-package #:cl-data-structures.common)


(defclass lazy-modification-operation-status
    (cl-ds:fundamental-modification-operation-status)
  ((%eager-status :accessor access-eager-status
                  :type eager-modification-operation-status
                  :initarg :eager-status)
   (%index :reader read-index
           :initarg :index
           :type non-negative-integer)
   (%lazy-instance :reader read-lazy-instance
                   :initarg :lazy-instance)))


(defclass box-container (cl-ds:fundamental-container)
  ((%content :initarg :content
             :type fundamental-container
             :accessor access-content)))


(defclass operations-box ()
  ((%operations :initform (make-array 0 :element-type '(-> (cl-ds:transactional) t)
                                        :adjustable t
                                        :fill-pointer 0)
                :type (vector (-> (cl-ds:transactional) t))
                :initarg :operations
                :accessor access-operations)
   (%current-version :initform 0
                     :type non-negative-integer
                     :initarg :current-version
                     :accessor access-current-version)))


(defclass lazy-box-container (cl-ds:lazy box-container)
  ((%operations :initform (make 'operations-box)
                :reader read-operations-box
                :initarg :operations)))


(-> force-version (lazy-box-container non-negative-integer) lazy-box-container)
(defun force-version (instance number)
  (nest
   (with-accessors ((box read-operations-box)) instance)
   (with-accessors ((operations access-operations)
                    (content access-content)
                    (current-version access-current-version))
       box)
   (let ((diff (- number current-version))))
   (when (>= diff 0))
   (let ((transactional-instance (cl-ds:become-transactional
                                  content))))
   (iterate
     (for i from 0 to diff)
     (for callback in-vector (access-operations instance))
     (funcall callback transactional-instance)
     (finally
      (setf
       operations (drop diff operations)

       current-version number

       content transactional-instance))))
  instance)


(-> force-all-versions (lazy-box-container) lazy-box-container)
(defun force-all-versions (instance)
  (nest
   (with-accessors ((box read-operations-box)
                    (content access-content))
       instance)
   (with-accessors ((operations access-operations)
                    (current-version access-current-version))
       box)
   (unless (zerop (fill-pointer operations)))
   (let ((transactional-instance (cl-ds:become-transactional
                                  content))))
   (iterate
     (for callback in-vector operations)
     (for i from 1)
     (funcall callback transactional-instance)
     (finally
      (setf
       operations (make-array 0 :element-type '(-> (cl-ds:transactional) t)
                                :adjustable t
                                :fill-pointer 0)

       current-version (+ current-version i)

       content transactional-instance))))
  instance)


(defun add-operation (container t-operation &rest args)
  (nest
   (with-accessors ((box read-operations-box)
                    (content access-content))
       container)
   (with-accessors ((current-version access-current-version)
                    (operations access-operations))
       box)
   (let* ((next-operations-box (make 'operations-box
                                     :operations (copy-array operations)
                                     :current-version (1+ current-version)))
          (next-instance (make 'lazy-box-container
                               :content content
                               :operations next-operations-box))
          (lazy-status (make 'lazy-modification-operation-status
                             :index (+ 1
                                       current-version
                                       (fill-pointer operations))
                             :lazy-instance next-instance))))
   (flet ((wrapper (instance)
            (let ((eager-status (nth-value 1 (apply t-operation instance args))))
              (unless (slot-boundp lazy-status '%eager-status)
                (setf (access-eager-status lazy-status) eager-status))
              nil)))
     (vector-push-extend #'wrapper (access-operations next-operations-box))
     (values next-instance
             lazy-status))))


(defmethod cl-ds:add ((container lazy-box-container) location new-value)
  (add-operation container #'cl-ds:add! location new-value))


(defmethod cl-ds:insert ((container lazy-box-container) location new-value)
  (add-operation container (lambda (instance)
                             (setf (cl-ds:at instance location) new-value))))


(defmethod cl-ds:update ((container lazy-box-container) location new-value)
  (add-operation container #'cl-ds:update! location new-value))


(defmethod cl-ds:erase ((container lazy-box-container) location)
  (add-operation container #'cl-ds:erase! location))


(defmethod cl-ds:size ((container lazy-box-container))
  (force-all-versions container)
  (cl-ds:size (access-content container)))


(defmethod cl-ds:at ((container lazy-box-container) location)
  (force-all-versions container)
  (cl-ds:at (access-content container) location))


(defmethod cl-ds:become-transactional ((container lazy-box-container))
  (force-all-versions container)
  (cl-ds:become-transactional (access-content container)))


(defmethod cl-ds:become-functional ((container lazy-box-container))
  (force-all-versions container)
  (cl-ds:become-functional (access-content container)))


(defmethod cl-ds:become-mutable ((container lazy-box-container))
  (force-all-versions container)
  (cl-ds:become-mutable (access-content container)))


(flet ((force-status (status)
         (unless (slot-boundp status '%eager-status)
           (force-version (read-lazy-instance status)
                          (read-index status)))
         (access-eager-status status)))

  (defmethod cl-ds:found ((status lazy-modification-operation-status))
    (cl-ds:found (force-status status)))

  (defmethod cl-ds:value ((status lazy-modification-operation-status))
    (cl-ds:value (force-status status))))


(defmethod cl-ds:become-lazy ((container cl-ds:fundamental-container))
  (make 'lazy-box-container
        :content (cl-ds:become-transactional container)))
