(in-package #:cl-data-structures.algorithms.meta)


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


(defclass fundamental-aggregator ()
  ())


(defclass aggregation-stage ()
  ((%name :initarg :name
          :reader read-name)
   (%construct-function :reader read-construct-function
                        :initarg :construct-function)
   (%function :initarg :function
              :reader read-function)
   (%state :reader read-state
           :initarg :state)))


(defun %stage (name construct-function)
  (make 'aggregation-stage
        :name name
        :construct-function construct-function))


(defmacro stage (name lambda-list &body body)
  `(%stage ,name (lambda ,lambda-list ,@body)))


(defclass linear-aggregator (fundamental-aggregator)
  ((%stages :initarg :stages
            :reader read-stages)
   (%arguments :initarg :arguments
               :initform nil
               :accessor access-arguments)
   (%range :initarg :range
           :accessor read-range)))


(defgeneric aggregate-accross (aggregator range)
  (:method ((aggregator fundamental-aggregator) range)
    (bind (((:slots %stages) aggregator)
           ((name function . state) (first %stages)))
      (declare (ignore name))
      (block end
        (cl-ds:across (lambda (x)
                        (when (aggregation-finished-p function state)
                          (return-from end))
                        (pass-to-aggregation aggregator x))
                      range)))))


(defgeneric initialize-stage (stage arguments))


(defgeneric pass-to-stage (stage item))


(defgeneric pass-to-aggregation (aggregator element))


(defgeneric construct-aggregator (range stages outer-fn arguments))


(defgeneric begin-aggregation (aggregator))


(defgeneric end-aggregation (aggregator))


(defgeneric extract-result (aggregator))


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
                                        &rest all
                                        &key key
                                        &allow-other-keys))


(defmethod apply-range-function ((range cl-ds:fundamental-range)
                                 (function layer-function)
                                 &rest all &key &allow-other-keys)
  (let ((clone (cl-ds:clone range)))
    (apply #'apply-layer clone function all)))


(defmethod apply-aggregation-function (range
                                       (function multi-aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (declare (ignore key)
           (optimize (debug 3)))
  (let* ((stages (append (apply #'multi-aggregation-stages function all)
                         (list (list* :result
                                      (lambda (range &rest rest)
                                        (apply #'apply-aggregation-function
                                               range
                                               function
                                               rest))))))
         (aggregator (construct-aggregator range stages nil all)))
    (begin-aggregation aggregator)
    (iterate
      (for stage on stages)
      (aggregate-accross aggregator range)
      (unless (endp (rest stage))
        (end-aggregation aggregator)))
    (extract-result aggregator)))


(defmethod apply-aggregation-function (range
                                       (function aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (let ((state (apply #'make-state function all)))
    (unless (aggregation-finished-p function state)
      (block end
        (cl-ds:across (lambda (x)
                        (when (aggregation-finished-p function state)
                          (return-from end))
                        (aggregate function
                                   state
                                   (if key (funcall key x) x)))
                      range)))
    (state-result function state)))


(defmethod extract-result ((aggregator linear-aggregator))
  (bind (((:slots %stages) aggregator)
         ((name function . state) (first %stages)))
    (state-result function state)))


(defmethod apply-aggregation-function ((stage aggregation-stage)
                                       (function aggregation-function)
                                       &rest all
                                       &key
                                       &allow-other-keys)
  (bind (((:slots %name %construct-function %state %function) stage)
         (state (apply #'make-state function all)))
    (setf %function function
          %state state)))


(defmethod apply-aggregation-function ((range linear-aggregator)
                                       (function aggregation-function)
                                       &rest all
                                       &key
                                       &allow-other-keys)
  (bind (((:slots %stages) range)
         (state (apply #'make-state function all)))
    (setf %stages
          (cons (list* (caar %stages) (list* function state))
                (rest %stages)))))


(defmethod pass-to-aggregation ((aggregator linear-aggregator)
                                element)
  (bind (((:slots %stages) aggregator)
         ((function . state) (cdr (first %stages))))
    (aggregate function state element)))


(defmethod end-aggregation ((aggregator linear-aggregator))
  (bind (((:slots %stages %arguments) aggregator)
         ((name . (function . state)) (first %stages))
         (stage-result (state-result function state))
         (rest (rest %stages)))
    (push stage-result %arguments)
    (push name %arguments)
    (setf %stages rest)
    (apply (cdar %stages)
           aggregator
           %arguments)
    stage-result))


(defun make-linear-aggregator (range arguments stages)
  (make 'linear-aggregator
        :range range
        :stages stages
        :arguments arguments))


(defmethod construct-aggregator ((range cl:sequence)
                                 (stages list)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator range arguments stages))


(defmethod construct-aggregator ((range cl:hash-table)
                                 (stages list)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator range arguments stages))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 (stages list)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator range arguments stages))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 (stages list)
                                 outer-fn
                                 (arguments list))
  (funcall outer-fn stages arguments))


(defmethod begin-aggregation ((aggregator linear-aggregator))
  (bind (((:slots %stages %arguments) aggregator)
         ((name . construct-function) (first %stages)))
    (declare (ignore name))
    (apply construct-function aggregator %arguments)))


(defmethod initialize-stage ((stage aggregation-stage) (arguments list))
  (bind (((:slots %name %construct-function %state) stage))
    (setf %state (apply %construct-function stage arguments))))


(defmethod pass-to-stage ((stage aggregation-stage) item)
  (bind (((:slots %name %construct-function %state %function) stage))
    (aggregate %function %state item)))


(defmethod extract-result ((stage aggregation-stage))
  (state-result (read-function stage) (read-state stage)))
