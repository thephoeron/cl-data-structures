(in-package #:cl-data-structures.common)


(defclass forward-tree-range (cl-ds:fundamental-forward-range)
  ((%forward-stack :type list
                   :accessor access-forward-stack
                   :initarg :forward-stack
                   :initform nil)
   (%obtain-value :type (-> ((-> () t) (-> (t) t)) (values t boolean))
                  :initarg :obtain-value
                  :reader read-obtain-value)
   (%initial-stack :type list
                   :initarg :initial-stack
                   :reader read-initial-stack
                   :initform nil)
   (%key :type (-> (t) t)
         :initarg :key
         :reader read-key)
   (%container :type cl-ds:fundamental-container
               :initarg :container
               :reader read-container)))


(defclass assignable-forward-tree-range
    (cl-ds:fundamental-assignable-forward-range forward-tree-range)
  ((%store-value :type (-> (t t) t)
                 :initarg :store-value
                 :reader read-store-value)))


(declaim (inline read-implementation))
(defun read-implementation (stack obtain-value)
  (if (null stack)
      (values stack nil nil)
      (bind (((:dflet push-to-stack (x))
              (push x stack))
             ((:dflet pop-stack ())
              (when (endp stack)
                (return-from read-implementation
                  (values stack nil nil)))
              (pop stack))
             (result (funcall obtain-value #'pop-stack #'push-to-stack)))
        (values stack result t))))


(defmethod cl-ds:across (function (range forward-tree-range))
  (let* ((stack (access-forward-stack range))
         (obtain-value (read-obtain-value range))
         (key (read-key range)))
    (iterate
      (until (null stack))
      (multiple-value-bind (new-stack value)
          (read-implementation stack obtain-value)
        (setf stack new-stack)
        (funcall function (funcall key value))))
    range))


(defmethod cl-ds:traverse (function (range forward-tree-range))
  (let* ((stack (access-forward-stack range))
         (obtain-value (read-obtain-value range))
         (key (read-key range)))
    (iterate
      (until (null stack))
      (multiple-value-bind (new-stack value)
          (read-implementation stack obtain-value)
        (setf stack new-stack
              (access-forward-stack range) stack)
        (funcall function (funcall key value))))
    range))


(defmethod cl-ds:peek-front ((range forward-tree-range))
  (bind (((:accessors (stack access-forward-stack)
                      (obtain-value read-obtain-value)
                      (key read-key))
          range)
         ((:values _ result found)
          (read-implementation stack obtain-value)))
    (values (when found (funcall key result)) found)))


(defmethod (setf cl-ds:peek-front)
    (new-value (range assignable-forward-tree-range))
  (bind (((:accessors (stack access-forward-stack)
                      (obtain-value read-obtain-value)
                      (key read-key)
                      (store-value read-store-value))
          range)
         ((:values _ result found)
          (read-implementation stack obtain-value)))
    (values (when found (funcall store-value result new-value)) found)))


(defmethod cl-ds:consume-front ((range forward-tree-range))
  (bind (((:accessors (stack access-forward-stack)
                      (obtain-value read-obtain-value)
                      (key read-key))
          range)
         ((:values new-stack result found)
          (read-implementation stack obtain-value)))
    (setf stack new-stack)
    (values (when found (funcall key result)) found)))


(defmethod cl-ds:drop-front ((range forward-tree-range) count)
  (let ((stack (access-forward-stack range))
        (obtain-value (read-obtain-value range)))
    (iterate
      (until (null stack))
      (repeat count)
      (setf stack (nth-value 1 (read-implementation stack
                                                    obtain-value))))
    (setf (access-forward-stack range) stack))
  range)


(defmethod cl-ds:clone ((range forward-tree-range))
  (make (type-of range)
        :container (read-container range)
        :forward-stack (mapcar #'cl-ds:clone (access-forward-stack range))
        :initial-stack (mapcar #'cl-ds:clone (read-initial-stack range))
        :obtain-value (read-obtain-value range)
        :key (read-key range)))


(defmethod cl-ds:reset! ((range forward-tree-range))
  (bind (((:slots %initial-stack %forward-stack) range))
    (setf %forward-stack (mapcar #'cl-ds:clone %initial-stack)))
  range)


(defmacro defmethod-with-stack ((method-name lambda-list stack stack-place)
                                &body body)
  `(defmethod ,method-name ,lambda-list
     (symbol-macrolet ((,stack ,stack-place))
       (labels ((,method-name ,(cl-ds.utils:method-lambda-list-to-function-lambda-list lambda-list)
                  ,@body))
         (,method-name ,@(cl-ds.utils:lambda-list-to-bindings lambda-list))))))


(defmacro defmethod-with-peek-stack ((method-name lambda-list stack init)
                                     &body body)
  `(defmethod ,method-name ,lambda-list
     (let ((,stack ,init))
       (labels ((,method-name ,(cl-ds.utils:method-lambda-list-to-function-lambda-list lambda-list)
                  ,@body))
         (,method-name ,@(cl-ds.utils:lambda-list-to-bindings lambda-list))))))


(defclass synchronized-queue-range (cl-ds:fundamental-forward-range)
  ((%queue :initform (lparallel.queue:make-queue)
           :reader read-queue)
   (%end-symbol :initform (gensym)
                :reader read-end-symbol)
   (%end :initform nil
         :accessor access-end)))


(defmethod cl-ds:consume-front ((range synchronized-queue-range))
  (bind (((:slots %queue %end %end-symbol) range))
    (if %end
        (values nil nil)
        (bind ((data (lparallel.queue:pop-queue %queue)))
          (if (eq data %end-symbol)
              (progn
                (setf %end t)
                (values nil nil))
              (values data t))))))


(defmethod cl-ds:peek-front ((range synchronized-queue-range))
  (bind (((:slots %queue %end %end-symbol) range))
    (if %end
        (values nil nil)
        (let ((data (lparallel.queue:peek-queue %queue)))
          (if (eq data %end-symbol)
              (progn
                (setf %end t)
                (values nil nil))
              (values data t))))))


(defmethod cl-ds:traverse (function (range synchronized-queue-range))
  (bind (((:slots %end %end-symbol %queue) range))
    (iterate
      (until %end)
      (for data = (lparallel.queue:pop-queue %queue))
      (if (eq data %end-symbol)
          (setf %end t)
          (funcall function data))))
  range)


(defmethod cl-ds:across (function (range synchronized-queue-range))
  (unless (access-end range)
    (bind (((:slots %end %end-symbol %queue) range)
           (next-queue (lparallel.queue:make-queue)))
      (iterate
        (for data = (lparallel.queue:pop-queue %queue))
        (unless (eq data %end-symbol)
          (funcall function data))
        (lparallel.queue:push-queue/no-lock data next-queue)
        (until (eq data %end-symbol)))
      (setf %queue next-queue)))
  range)


(defun put-into-queue (container item)
  (declare (type synchronized-queue-range container))
  (let ((queue (read-queue container))
        (end (access-end container)))
    (when end
      (error 'cl-ds:operation-not-allowed :text "Can't put-into closed queue."))
    (lparallel.queue:push-queue item queue))
  container)


(defun close-queue (container)
  (declare (type synchronized-queue-range container))
  (let ((queue (read-queue container))
        (end-symbol (read-end-symbol container)))
    (lparallel.queue:push-queue end-symbol queue))
  container)


(defun make-synchronized-queue-range ()
  (make 'synchronized-queue-range))
