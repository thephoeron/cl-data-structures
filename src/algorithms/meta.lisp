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
            :accessor access-stages)
   (%arguments :initarg :arguments
               :initform nil
               :accessor access-arguments)
   (%accumulator :initform nil
                 :accessor access-accumulator)
   (%range :initarg :range
           :accessor read-range)))


(defun aggregate-accross (stage range)
  (cl-ds:across (lambda (x)
                  (when (pass-to-stage stage x)
                    (return-from aggregate-accross)))
                range))


(defgeneric initialize-stage (stage arguments))


(defgeneric pass-to-stage (stage item))


(defgeneric pass-to-aggregation (aggregator element))


(defgeneric construct-aggregator (range stages outer-fn arguments))


(defgeneric begin-aggregation (aggregator))


(defgeneric end-aggregation (aggregator))


(defgeneric extract-result (aggregator))


(defgeneric extract-result-with-stage (stage aggregator))


(defgeneric copy-stage (stage))


(defgeneric aggregator-finished-p (aggregator))


(defgeneric begin-aggregation-with-stage (stage aggregator))


(defgeneric end-aggregation-with-stage (stage aggregator))


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


(defgeneric handle-stage (stage range))


(defmethod handle-stage ((stage )))


(defmethod apply-range-function ((range cl-ds:fundamental-range)
                                 (function layer-function)
                                 &rest all &key &allow-other-keys)
  (let ((clone (cl-ds:clone range)))
    (apply #'apply-layer clone function all)))


(defmethod apply-aggregation-function (range
                                       (function multi-aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (declare (ignore key))
  (let* ((stages (append (apply #'multi-aggregation-stages function all)))
         (aggregator (construct-aggregator range stages nil all)))
    (iterate
      (for stage on stages)
      (begin-aggregation aggregator)
      (for elt = (first stage))
      (aggregate-accross elt range)
      (end-aggregation aggregator))
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
  (bind (((:slots %accumulator) aggregator))
    %accumulator))


(defmethod apply-aggregation-function ((stage aggregation-stage)
                                       (function aggregation-function)
                                       &rest all
                                       &key
                                       &allow-other-keys)
  (bind (((:slots %name %construct-function %state %function) stage)
         (state (apply #'make-state function all)))
    (setf %function function
          %state state)))


(defmethod pass-to-aggregation ((aggregator linear-aggregator)
                                element)
  (bind (((:slots %stages) aggregator)
         (stage (first %stages))
         (finished (pass-to-stage stage element)))
    finished))


(defmethod end-aggregation ((aggregator linear-aggregator))
  (end-aggregation-with-stage (~> aggregator read-stages first)
                              aggregator))


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
  (begin-aggregation-with-stage (first (access-stages aggregator))
                                aggregator))


(defmethod initialize-stage ((stage aggregation-stage) (arguments list))
  (bind (((:slots %name %construct-function %state) stage))
    (setf %state (apply %construct-function stage arguments))))


(defmethod initialize-stage ((stage cl:function) (arguments list))
  (apply stage arguments))


(defmethod pass-to-stage ((stage aggregation-stage) item)
  (bind (((:slots %name %construct-function %state %function) stage))
    (lret ((finished (aggregation-finished-p %function %state)))
      (unless finished
        (aggregate %function %state item)))))


(defmethod extract-result ((stage aggregation-stage))
  (state-result (read-function stage) (read-state stage)))


(defmethod copy-stage ((stage aggregation-stage))
  (make (type-of stage)
        :name (read-name stage)
        :construct-function (read-construct-function stage)))


(defmethod copy-stage ((stage cl:function))
  stage)


(defmethod begin-aggregation-with-stage ((stage cl:function)
                                         (aggregator linear-aggregator))
  (bind (((:slots %accumulator %arguments) aggregator))
    (setf %accumulator (apply stage %arguments))
    (push %accumulator %arguments)))


(defmethod end-aggregation-with-stage ((stage cl:function)
                                       (aggregator linear-aggregator))
  (bind (((:slots %stages) aggregator))
    (pop %stages)))


(defmethod begin-aggregation-with-stage ((stage aggregation-stage)
                                         (aggregator linear-aggregator))
  (bind (((:slots %accumulator %arguments) aggregator))
    (initialize-stage stage %arguments)))


(defmethod end-aggregation-with-stage ((stage aggregation-stage)
                                       (aggregator linear-aggregator))
  (bind (((:slots %stages %arguments %accumulator) aggregator)
         (result (extract-result stage)))
    (setf %accumulator result)
    (pop %stages)
    (push result %arguments)
    (when (slot-boundp stage '%name)
      (push (read-name stage) %arguments))))
