(in-package #:cl-data-structures.math)


(defclass fit-polymional-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defun construct-gradient-arguments-list (arguments data-vars element)
  (iterate
    (for (label call . rest)
         initially data-vars
         then rest)
    (push (coerce (funcall call element) 'double-float)
          arguments)
    (push label arguments)
    (until (endp rest)))
  arguments)


(defgeneric fit-polymional (range expression
                            data-vars matched-vars
                            learning-rate
                            &key key)
  (:generic-function-class fit-polymional-function)
  (:method (range expression
            data-vars matched-vars
            learning-rate
            &key (key #'identity))
    (cl-ds.alg:apply-aggregation-function range
                                          #'fit-polymional
                                          :expression expression
                                          :data-vars data-vars
                                          :matched-vars (copy-list matched-vars)
                                          :learning-rate learning-rate
                                          :key key)))


(defclass fit-polymional-state ()
  ((%error-sum :initform 0
               :accessor access-average-error)
   (%gradient-sum :initform (make-hash-table)
                  :reader read-gradient-sum)
   (%count :initform 0
           :accessor access-count)
   (%data-vars :initarg :data-vars
               :reader read-range-vars)
   (%learning-rate :initarg :learning-rate
                   :reader read-learning-rate)
   (%gradient-function :initarg :gradient-function
                       :reader read-function)
   (%matched-vars :initarg :matched-vars
                  :reader read-matched-vars)))


(defmethod cl-ds.alg:make-state ((function fit-polymional-function)
                                 &rest all
                                 &key
                                   expression data-vars
                                   matched-vars learning-rate
                                   &allow-other-keys)
  (declare (ignore all))
  (make 'fit-polymional-state
        :data-vars data-vars
        :matched-vars matched-vars
        :gradient-function (econd
                             ((listp expression)
                              (~> expression
                                  cl-ds.math.grad:make-gradient-expression
                                  cl-ds.math.grad:compile-gradient-expression))
                             ((functionp expression)
                              expression))
        :learning-rate learning-rate))


(defmethod cl-ds.alg:state-result ((function fit-polymional-function)
                                   state)
  (bind (((:slots %error-sum %count %gradient-sum
                  %matched-vars %learning-rate)
          state)
         (average-error (/ %error-sum %count))
         (vars (iterate
                 (for (name value . rest)
                      initially %matched-vars
                      then rest)
                 (for gradient = (gethash name %gradient-sum))
                 (collect (- value (* %learning-rate gradient)) at start)
                 (collect name at start)
                 (until (endp rest)))))
    (values vars average-error)))


(defmethod cl-ds.alg:aggregate ((function fit-polymional-function)
                                state
                                element)
  (bind (((:slots %data-vars %matched-vars %gradient-function %error-sum
                  %learning-rate %count %gradient-sum)
          state)
         (arguments (construct-gradient-arguments-list %matched-vars
                                                       %data-vars
                                                       element))
         ((:values result gradients) (apply %gradient-function arguments)))
    (incf %count)
    (incf %error-sum (abs result))
    (iterate
      (for (name gradient . rest) initially gradients then rest)
      (incf (gethash name %gradient-sum 0) gradient)
      (until (endp rest)))))


(iterate
  (with gradient = (~> '(- y (+ (* a b) (sin x)))
                       cl-ds.math.grad:make-gradient-expression))
  (with values = (list :b 15.1d0 :a 12.0d0))
  (with data = (map 'vector
                    (lambda (x) (list x (+ (+ 5.0 2.4) (sin x))))
                    (iota 1000 :start 0)))
  (with extract = (list :x #'first :y #'second))
  (for (values new-values error) =
       (fit-polymional data
                       gradient
                       extract
                       values
                       0.001))
  (setf values new-values)
  (print error)
  (print new-values)
  (repeat 2))
