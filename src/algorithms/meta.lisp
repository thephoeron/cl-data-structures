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
  ((%arguments :initarg :arguments
               :accessor access-arguments
               :initform nil)))


(defclass fundamental-aggregation-stage ()
  ())


(defclass aggregation-stage (fundamental-aggregation-stage)
  ((%name :initarg :name
          :reader read-name)
   (%construct-function :reader read-construct-function
                        :initarg :construct-function)
   (%function :initarg :function
              :reader read-function)
   (%state :reader read-state
           :initarg :state)))


(defclass reduce-stage (fundamental-aggregation-stage)
  ((%name :initarg :name
          :reader read-name)
   (%function :initarg :function
              :reader read-function)
   (%arguments :initform nil
               :accessor access-arguments)
   (%state :reader read-state
           :initarg :state)))


(defun %stage (name construct-function)
  (make 'aggregation-stage
        :name name
        :construct-function construct-function))


(defun %reduce-stage (name state-init function)
  (make 'reduce-stage
        :name name
        :function function
        :state state-init))


(defmacro stage (name lambda-list &body body)
  `(%stage ,name (lambda ,lambda-list ,@body)))


(defmacro reduce-stage (name init-form lambda-list &body body)
  `(%reduce-stage ,name ,init-form (lambda ,lambda-list ,@body)))


(defclass linear-aggregator (fundamental-aggregator)
  ((%function :initarg :function
              :reader read-function)
   (%state :initarg :state
           :accessor read-state)
   (%ended :initform nil
           :accessor access-ended)))


(defclass multi-stage-linear-aggregator (fundamental-aggregator)
  ((%stages :initarg :stages
            :accessor access-stages)
   (%accumulator :initform nil
                 :accessor access-accumulator)))


(defgeneric apply-aggregation-function (range function
                                        &rest all
                                        &key key
                                        &allow-other-keys))


(defgeneric expects-content (aggregator))


(defgeneric expects-content-with-stage (stage aggregator))


(defgeneric initialize-stage (stage arguments))


(defgeneric pass-to-aggregation (aggregator element))


(defgeneric pass-to-aggregation-with-stage (stage aggregator element))


(defgeneric construct-aggregator (range function outer-fn arguments))


(defgeneric construct-aggregator-with-stages (range stages outer-fn arguments))


(defgeneric begin-aggregation (aggregator))


(defgeneric end-aggregation (aggregator))


(defgeneric extract-result (aggregator))


(defgeneric extract-result-with-stage (stage aggregator))


(defgeneric aggregator-completed-stage (aggregator))


(defgeneric aggregator-completed-stage-with-stage (stage aggregator))


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


(defgeneric aggregate (function state element))


(defgeneric state-result (function state)
  (:method ((function aggregation-function) state)
    state))


(defgeneric apply-range-function (range function
                                  &rest all
                                  &key &allow-other-keys))


(defmethod apply-range-function ((range cl-ds:fundamental-range)
                                 (function layer-function)
                                 &rest all &key &allow-other-keys)
  (let ((clone (cl-ds:clone range)))
    (apply #'apply-layer clone function all)))


(defmethod extract-result ((aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator) aggregator))
    %accumulator))


(defmethod extract-result ((aggregator linear-aggregator))
  (bind (((:slots %state %function) aggregator))
    (state-result %function %state)))


(defmethod pass-to-aggregation ((aggregator multi-stage-linear-aggregator)
                                element)
  (bind (((:slots %stages) aggregator)
         (stage (first %stages))
         (finished (pass-to-aggregation-with-stage stage aggregator element)))
    finished))


(defmethod end-aggregation ((aggregator multi-stage-linear-aggregator))
  (end-aggregation-with-stage (~> aggregator access-stages first)
                              aggregator))


(defun make-linear-aggregator (function arguments)
  (make 'linear-aggregator
        :function function
        :arguments arguments))


(defun make-multi-stage-linear-aggregator (arguments stages)
  (make 'multi-stage-linear-aggregator
        :stages stages
        :arguments arguments))


(defmethod begin-aggregation ((aggregator multi-stage-linear-aggregator))
  (begin-aggregation-with-stage (first (access-stages aggregator))
                                aggregator))


(defmethod initialize-stage ((stage aggregation-stage) (arguments list))
  (bind (((:slots %name %construct-function %state) stage))
    (apply %construct-function stage arguments)))


(defmethod initialize-stage ((stage cl:function) (arguments list))
  nil)


(defmethod extract-result ((stage aggregation-stage))
  (state-result (read-function stage) (read-state stage)))


(defmethod begin-aggregation-with-stage ((stage cl:function)
                                         (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator %arguments) aggregator))
    (setf %accumulator (apply stage %arguments))
    (push %accumulator %arguments)))


(defmethod end-aggregation-with-stage ((stage cl:function)
                                       (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %stages) aggregator))
    (pop %stages)))


(defmethod begin-aggregation-with-stage ((stage aggregation-stage)
                                         (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator %arguments) aggregator))
    (initialize-stage stage %arguments)))


(defmethod end-aggregation-with-stage ((stage aggregation-stage)
                                       (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %stages %arguments %accumulator) aggregator)
         (result (extract-result stage)))
    (setf %accumulator result)
    (pop %stages)
    (push result %arguments)
    (when (slot-boundp stage '%name)
      (push (read-name stage) %arguments))))


(defmethod pass-to-aggregation-with-stage ((stage aggregation-stage)
                                           (aggregator multi-stage-linear-aggregator)
                                           element)
  (nest
   (bind (((:slots %name %construct-function %state %function) stage)))
   (aggregate %function %state item)))


(defmethod apply-range-function ((range cl-ds:fundamental-range)
                                 (function layer-function)
                                 &rest all &key &allow-other-keys)
  (let ((clone (cl-ds:clone range)))
    (apply #'apply-layer clone function all)))


(defmethod apply-aggregation-function (range
                                       (function multi-aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (declare (ignore key))
  (let* ((aggregator (construct-aggregator range function nil all)))
    (iterate
      (until (aggregator-finished-p aggregator))
      (begin-aggregation aggregator)
      (until (aggregator-finished-p aggregator))
      (when (cl-ds.alg.meta:expects-content aggregator)
        (cl-ds:across (lambda (x)
                        (pass-to-aggregation range x))
                      range))
      (end-aggregation aggregator))
    (extract-result aggregator)))


(defmethod extract-result ((aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator) aggregator))
    %accumulator))


(defmethod extract-result ((aggregator linear-aggregator))
  (bind (((:slots %state %function) aggregator))
    (state-result %function %state)))


(defmethod pass-to-aggregation ((aggregator multi-stage-linear-aggregator)
                                element)
  (bind (((:slots %stages) aggregator)
         (stage (first %stages)))
    (pass-to-aggregation-with-stage stage aggregator element)))


(defmethod pass-to-aggregation ((aggregator linear-aggregator)
                                element)
  (bind (((:slots %function %state) aggregator))
    (aggregate %function %state element)))


(defmethod end-aggregation ((aggregator multi-stage-linear-aggregator))
  (end-aggregation-with-stage (~> aggregator access-stages first)
                              aggregator))


(defmethod construct-aggregator ((range cl:sequence)
                                 (function multi-aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-multi-stage-linear-aggregator
   arguments (apply #'multi-aggregation-stages function arguments)))


(defmethod construct-aggregator ((range cl:hash-table)
                                 (function multi-aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-multi-stage-linear-aggregator
   arguments (apply #'multi-aggregation-stages function arguments)))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 (function aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator function arguments))


(defmethod construct-aggregator ((range cl:sequence)
                                 (function aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator function arguments))


(defmethod construct-aggregator ((range cl:sequence)
                                 (function aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator function arguments))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 (function multi-aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-multi-stage-linear-aggregator
   arguments (apply #'multi-aggregation-stages function arguments)))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 (function aggregation-function)
                                 outer-fn
                                 (arguments list))
  (funcall outer-fn function arguments))


(defmethod begin-aggregation ((aggregator multi-stage-linear-aggregator))
  (begin-aggregation-with-stage (first (access-stages aggregator))
                                aggregator))


(defmethod initialize-stage ((stage aggregation-stage) (arguments list))
  (bind (((:slots %name %construct-function %state) stage))
    (apply %construct-function stage arguments)))


(defmethod initialize-stage ((stage cl:function) (arguments list))
  nil)


(defmethod extract-result ((stage aggregation-stage))
  (state-result (read-function stage) (read-state stage)))


(defmethod begin-aggregation-with-stage ((stage cl:function)
                                         (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %stages %accumulator %arguments) aggregator))
    (setf %accumulator (apply stage %arguments))
    (push %accumulator %arguments)
    (pop %stages)))


(defmethod end-aggregation-with-stage ((stage cl:function)
                                       (aggregator multi-stage-linear-aggregator))
  nil)


(defmethod begin-aggregation-with-stage ((stage aggregation-stage)
                                         (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator %arguments) aggregator))
    (initialize-stage stage %arguments)))


(defmethod end-aggregation-with-stage ((stage aggregation-stage)
                                       (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %stages %arguments %accumulator) aggregator)
         (result (extract-result stage)))
    (setf %accumulator result)
    (pop %stages)
    (push result %arguments)
    (when (slot-boundp stage '%name)
      (push (read-name stage) %arguments))))


(defmethod pass-to-aggregation-with-stage ((stage aggregation-stage)
                                           (aggregator multi-stage-linear-aggregator)
                                           item)
  (bind (((:slots %name %construct-function %state %function) stage))
    (aggregate %function %state item)))


(defmethod begin-aggregation ((aggregator linear-aggregator))
  (when (slot-boundp aggregator '%state)
    (error "Can't begin-aggregation twice for linear-aggregator"))
  (bind (((:slots %function %state %arguments) aggregator))
    (setf %state (apply #'make-state %function %arguments))
    aggregator))


(defmethod end-aggregation ((aggregator linear-aggregator))
  (setf (access-ended aggregator) t))


(defmethod aggregator-finished-p ((aggregator linear-aggregator))
  (access-ended aggregator))


(defmethod aggregator-finished-p ((aggregator multi-stage-linear-aggregator))
  (endp (access-stages aggregator)))


(defmethod apply-aggregation-function ((stage aggregation-stage)
                                       (function aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (declare (ignore key))
  (bind (((:slots %function %state) stage))
    (setf %state (apply #'make-state function all)
          %function function)))


(defmethod begin-aggregation-with-stage ((stage reduce-stage)
                                         (aggregator multi-stage-linear-aggregator))
  (setf (access-arguments stage) (access-arguments aggregator)))


(defmethod extract-result ((stage reduce-stage))
  (read-state stage))


(defmethod end-aggregation-with-stage ((stage reduce-stage)
                                       (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %stages %arguments %accumulator) aggregator))
    (setf %accumulator (extract-result stage))
    (pop %stages)
    (push %accumulator %arguments)
    (when (slot-boundp stage '%name)
      (push (read-name stage) %arguments))))


(defmethod pass-to-aggregation-with-stage ((stage reduce-stage)
                                           (aggregator multi-stage-linear-aggregator)
                                           item)
  (bind (((:slots %state %function %arguments) stage))
    (setf %state (apply %function %state
                        item %arguments))))


(defmethod expects-content ((aggregator linear-aggregator))
  t)


(defmethod expects-content ((aggregator multi-stage-linear-aggregator))
  (expects-content-with-stage (first (access-stages aggregator))
                              aggregator))


(defmethod expects-content-with-stage ((stage fundamental-aggregation-stage)
                                       (aggregator multi-stage-linear-aggregator))
  t)

(defmethod expects-content-with-stage ((stage cl:function)
                                       (aggregator multi-stage-linear-aggregator))
  nil)
