(in-package #:cl-data-structures.algorithms)


(defgeneric apply-layer (range function &rest all &key &allow-other-keys))


(defclass range-function (closer-mop:standard-generic-function)
  ())


(defclass layer-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass transformation!-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass aggregation-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass multi-aggregation-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric multi-aggregation-stages (aggregation-function
                                      &rest all &key &allow-other-keys)
  (:method ((function aggregation-function) &rest all &key &allow-other-keys)
    (declare (ignore all))
    nil))


(defgeneric make-state (aggregation-function
                        &rest all
                        &key &allow-other-keys))


(defgeneric aggregation-finished-p (aggregation-function state)
  (:method ((function aggregation-function) state)
    nil))


(defgeneric aggregate (function state element))


(defgeneric state-result (function state)
  (:method ((function aggregation-function) state)
    state))


(defmacro gather-prior-states (fn range into)
  (with-gensyms (!result !name !stage)
    (once-only (fn range)
      `(iterate
         (for (,!name . ,!stage) in (multi-aggregation-stages ,fn ,into))
         (for ,!result = (funcall ,!stage ,range))
         (push ,!result ,into)
         (push ,!name ,into)))))


(defgeneric apply-range-function (range function
                                  &rest all
                                  &key &allow-other-keys))


(defgeneric apply-aggregation-function (range function
                                        &rest all &key key &allow-other-keys))


(defmethod apply-range-function ((range cl-ds:fundamental-range)
                                 (function layer-function)
                                 &rest all &key &allow-other-keys)
  (let ((clone (cl-ds:clone range)))
    (apply #'apply-layer clone function all)))


(defclass states-collector (cl-ds:traversable)
  ((%args :initform nil
          :initarg :args
          :accessor access-args)
   (%label :initform nil
           :accessor access-label)
   (%original :initarg :original
              :reader read-original)))


(defmethod apply-aggregation-function ((range states-collector)
                                       (function aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (declare (ignore all))
  (let ((state (apply #'make-state function (access-args range))))
    (unless (aggregation-finished-p function state)
      (block end
        (cl-ds:traverse (lambda (x)
                          (aggregate function
                                     state
                                     (if key (funcall key x) x))
                          (when (aggregation-finished-p function state)
                            (return-from end)))
                        (read-original range))))
    (push (state-result function state) (access-args range))
    (push (access-label range) (access-args range))))


(defmethod apply-aggregation-function(range
                                      (function multi-aggregation-function)
                                      &rest all &key key &allow-other-keys)
  (declare (ignore key))
  (let ((stages (apply #'multi-aggregation-stages function all)))
    (unless (endp stages)
      (iterate
        (with collector = (make 'states-collector :args all :original range))
        (for (name . stage) in stages)
        (setf (access-label collector) name)
        (funcall stage collector)
        (finally (return (apply #'call-next-method
                                range
                                function
                                (access-args collector))))))))


(defmethod apply-aggregation-function (range
                                       (function aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (let ((state (apply #'make-state function all)))
    (unless (aggregation-finished-p function state)
      (block end
        (cl-ds:traverse (lambda (x)
                          (aggregate function
                                     state
                                     (if key (funcall key x) x))
                          (when (aggregation-finished-p function state)
                            (return-from end)))
                        range)))
    (state-result function state)))
