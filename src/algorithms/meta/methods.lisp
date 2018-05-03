(in-package #:cl-data-structures.algorithms.meta)

#|
Top level aggregator protocol.
|#

(defmethod extract-result ((aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator) aggregator))
    %accumulator))


(defmethod extract-result ((aggregator linear-aggregator))
  (bind (((:slots %state %function) aggregator))
    (state-result %function %state)))


(defmethod extract-result :before ((aggregator fundamental-aggregator))
  (unless (aggregator-finished-p aggregator)
    (error 'cl-ds:operation-not-allowed
           :text "Can't extract result from unfinished aggregator")))


(defmethod extract-result ((stage aggregation-stage))
  (state-result (read-function stage) (read-state stage)))


(defmethod begin-aggregation :before ((aggregator fundamental-aggregator))
  (when (aggregator-finished-p aggregator)
    (error 'cl-ds:operation-not-allowed
           :text "Can't extract result from unfinished aggregator")))


(defmethod begin-aggregation ((aggregator multi-stage-linear-aggregator))
  (begin-aggregation-with-stage (first (access-stages aggregator))
                                aggregator))


(defmethod begin-aggregation ((aggregator linear-aggregator))
  (bind (((:slots %state %arguments %function) aggregator))
    (setf %state (apply #'make-state %function %arguments)))
  nil)


(defmethod end-aggregation :before ((aggregator fundamental-aggregator))
  (when (aggregator-finished-p aggregator)
    (error 'cl-ds:operation-not-allowed
           :text "Can't end-aggregation on finished aggregator")))


(defmethod end-aggregation ((aggregator multi-stage-linear-aggregator))
  (end-aggregation-with-stage (~> aggregator access-stages first)
                              aggregator))


(defmethod end-aggregation ((aggregator linear-aggregator))
  (setf (access-ended aggregator) t))


(defmethod aggregator-finished-p ((aggregator linear-aggregator))
  (access-ended aggregator))


(defmethod aggregator-finished-p ((aggregator multi-stage-linear-aggregator))
  (~> aggregator access-stages endp))


(defmethod pass-to-aggregation ((aggregator multi-stage-linear-aggregator)
                                element)
  (bind (((:slots %stages %key) aggregator)
         (stage (first %stages)))
    (pass-to-aggregation-with-stage stage aggregator
                                    element)))


(defmethod pass-to-aggregation :before ((aggregator fundamental-aggregator)
                                        element)
  (when (aggregator-finished-p aggregator)
    (error 'cl-ds:operation-not-allowed
           :text "Can't pass element to aggregator that is already finished.")))


(defmethod pass-to-aggregation ((aggregator linear-aggregator)
                                element)
  (bind (((:slots %function %state %key) aggregator))
    (aggregate %function %state (funcall %key element))))


(defmethod construct-aggregator ((range cl:sequence)
                                 key
                                 (function multi-aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (setf key (or key #'identity))
  (make-multi-stage-linear-aggregator
   arguments key (apply #'multi-aggregation-stages function arguments)))


(defmethod construct-aggregator ((range cl:hash-table)
                                 key
                                 (function multi-aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (setf key (or key #'identity))
  (make-multi-stage-linear-aggregator
   arguments key (apply #'multi-aggregation-stages function arguments)))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 key
                                 (function aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator function arguments key))


(defmethod construct-aggregator ((range cl:sequence)
                                 key
                                 (function aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator function arguments key))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 key
                                 (function multi-aggregation-function)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-multi-stage-linear-aggregator
   arguments key (apply #'multi-aggregation-stages function arguments)))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 key
                                 (function aggregation-function)
                                 outer-fn
                                 (arguments list))
  (lret ((result (funcall outer-fn)))
    (setf (slot-value result '%key) key)))


(defmethod construct-aggregator ((range cl:sequence)
                                 key
                                 (function aggregation-function)
                                 outer-fn
                                 (arguments list))
  (lret ((result (funcall outer-fn)))
    (setf (slot-value result '%key) key)))


(defmethod expects-content-p ((aggregator linear-aggregator))
  t)


(defmethod expects-content-p ((aggregator multi-stage-linear-aggregator))
  (expects-content-with-stage-p (first (access-stages aggregator))
                                aggregator))

#|
Stage level aggregator protocol.
|#

(defmethod initialize-stage ((stage aggregation-stage) (arguments list))
  (bind (((:slots %name %construct-function %state) stage))
    (apply %construct-function stage arguments)))


(defmethod initialize-stage ((stage cl:function) (arguments list))
  nil)


(defmethod begin-aggregation-with-stage ((stage cl:function)
                                         (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator %arguments) aggregator))
    (setf %accumulator (apply stage %arguments))
    (push %accumulator %arguments)))


(defmethod begin-aggregation-with-stage ((stage aggregation-stage)
                                         (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator %arguments) aggregator))
    (initialize-stage stage %arguments)))


(defmethod begin-aggregation-with-stage ((stage cl:function)
                                         (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator %arguments) aggregator))
    (setf %accumulator (apply stage %arguments))
    (push %accumulator %arguments)))


(defmethod begin-aggregation-with-stage ((stage aggregation-stage)
                                         (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %accumulator %arguments) aggregator))
    (initialize-stage stage %arguments)))


(defmethod begin-aggregation-with-stage ((stage reduce-stage)
                                         (aggregator multi-stage-linear-aggregator))
  nil)


(defmethod end-aggregation-with-stage ((stage cl:function)
                                       (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %stages) aggregator))
    (pop %stages)))


(defmethod end-aggregation-with-stage ((stage aggregation-stage)
                                       (aggregator multi-stage-linear-aggregator))
  (bind (((:slots %stages %arguments %accumulator) aggregator)
         (result (extract-result stage)))
    (setf %accumulator result)
    (pop %stages)
    (push result %arguments)
    (when (slot-boundp stage '%name)
      (push (read-name stage) %arguments))))


(defmethod end-aggregation-with-stage ((stage reduce-stage)
                                       (aggregator multi-stage-linear-aggregator))
  (setf (access-accumulator aggregator) (access-state stage)
        (access-stages aggregator) (~> aggregator access-stages rest))
  (push (access-state stage) (access-arguments aggregator))
  (when (slot-boundp stage '%name)
    (push (read-name stage) (access-arguments aggregator))))


(defmethod pass-to-aggregation-with-stage ((stage aggregation-stage)
                                           (aggregator multi-stage-linear-aggregator)
                                           element)
  (bind (((:slots %name %key %construct-function %state %function) stage))
    (aggregate %function %state (funcall %key element))))


(defmethod pass-to-aggregation-with-stage ((stage aggregation-stage)
                                           (aggregator multi-stage-linear-aggregator)
                                           element)
  (bind (((:slots %name %key %construct-function %state %function) stage))
    (aggregate %function %state (funcall %key element))))


(defmethod pass-to-aggregation-with-stage ((stage reduce-stage)
                                           (aggregator multi-stage-linear-aggregator)
                                           element)
  (apply (read-function stage)
         (access-state stage)
         element
         (access-arguments aggregator)))


(defmethod initialize-stage ((stage aggregation-stage) (arguments list))
  (bind (((:slots %name %construct-function %state) stage))
    (apply %construct-function stage arguments)))


(defmethod initialize-stage ((stage cl:function) (arguments list))
  nil)


(defmethod expects-content-with-stage-p ((stage fundamental-aggregation-stage)
                                       (aggregator multi-aggregator))
  t)


(defmethod expects-content-with-stage-p ((stage cl:function)
                                       (aggregator multi-aggregator))
  nil)

#|
Range function invokaction protocol.
|#

(defmethod apply-range-function ((range cl-ds:fundamental-range)
                                 (function layer-function)
                                 &rest all &key &allow-other-keys)
  (let ((clone (cl-ds:clone range)))
    (apply #'apply-layer clone function all)))


(defmethod apply-range-function ((range cl:sequence)
                                 (function layer-function)
                                 &rest all &key &allow-other-keys)
  (apply #'apply-layer (cl-ds:whole-range range) function all))


(defmethod apply-aggregation-function (range
                                       (function aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (let* ((aggregator (construct-aggregator range key function nil all)))
    (apply #'apply-aggregation-function-with-aggregator
           aggregator range function all)))


(defmethod apply-aggregation-function-with-aggregator ((aggregator fundamental-aggregator)
                                                       range
                                                       (function aggregation-function)
                                                       &rest all &key &allow-other-keys)
  (declare (ignore all))
  (iterate
    (until (aggregator-finished-p aggregator))
    (begin-aggregation aggregator)
    (until (aggregator-finished-p aggregator))
    (when (cl-ds.alg.meta:expects-content-p aggregator)
      (cl-ds:across (lambda (x)
                      (pass-to-aggregation aggregator
                                           x))
                    range))
    (end-aggregation aggregator))
  (extract-result aggregator))


(defmethod apply-aggregation-function ((stage aggregation-stage)
                                       (function aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (bind (((:slots %function %key %state) stage))
    (setf %state (apply #'make-state function all)
          %function function
          %key key)))
