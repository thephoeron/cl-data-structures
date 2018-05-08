(in-package #:cl-data-structures.algorithms.meta)


(eval-always
  (defun extract-parameters-as-key-arguments (parsed-lambda-list)
    (let ((mandatory nil)
          (optional nil))
      (iterate
        (with required = (aref parsed-lambda-list 0))
        (with optional = (aref parsed-lambda-list 1))
        (for argument in (append required optional))
        (when (eq argument 'range)
          (next-iteration))
        (push argument mandatory)
        (push (make-keyword argument) mandatory))
      (iterate
        (with keywords = (aref parsed-lambda-list 3))
        (for ((keyword-name name) init suppliedp) in keywords)
        (if suppliedp
            (push `(if ,suppliedp (list ,keyword-name ,name) nil) optional)
            (progn (push name mandatory)
                   (push keyword-name mandatory))))
      (values mandatory optional)))

  (defun aggregation-function-class-form (function-class)
    `(defclass ,function-class (cl-ds.alg.meta:aggregation-function)
       ()
       (:metaclass closer-mop:funcallable-standard-class)))

  (defun aggregation-function-defgeneric-form (function-name function-class required-arguments
                                               optional-arguments generic-lambda-list method-lambda-list
                                               all)
    `(defgeneric ,function-name ,generic-lambda-list
       (:generic-function-class ,function-class)
       (:method ,method-lambda-list
         ,(cond+ ((null all) optional-arguments)
            ((t nil) `(apply-aggregation-function range
                                                  (function ,function-name)
                                                  ,@required-arguments))
            ((nil t) `(apply #'apply-aggregation-function range
                             (function ,function-name)
                             ,@required-arguments
                             (append ,@optional-arguments ,all)))
            ((nil nil) `(apply #'apply-aggregation-function range
                               (function ,function-name)
                               ,@required-arguments
                               ,all))
            ((t t) `(apply #'apply-aggregation-function range
                           (function ,function-name)
                           ,@required-arguments
                           (append ,@optional-arguments)))))))

  (defun aggregation-function-make-state-form (function-class function-state init-form)
    (bind (((lambda-list . body) init-form)
           (length (length function-state))
           (result (gensym))
           (function-object (gensym)))
      `(defmethod cl-ds.alg.meta:make-state ((,function-object ,function-class)
                                             ,@lambda-list)
         (lret ((,result (make-array ,length)))
           (symbol-macrolet ,(mapcar (lambda (i x) `(,x (aref ,result ,i)))
                                     (iota length)
                              function-state)
             ,@body)))))

  (defun aggregation-function-aggregate-form (function-class function-state aggregate-form)
    (bind (((lambda-list . body) aggregate-form)
           (length (length function-state))
           (state (gensym))
           (function-object (gensym)))
      `(defmethod cl-ds.alg.meta:aggregate ((,function-object ,function-class)
                                            ,state ,@lambda-list)
         (symbol-macrolet ,(mapcar
                            (lambda (i x) `(,x (aref ,state ,i)))
                            (iota length)
                            function-state)
           ,@body
           ,state))))

  (defun aggregation-function-result-form (function-class function-state result-form)
    (bind ((length (length function-state))
           (state (gensym))
           (function-object (gensym)))
      `(defmethod cl-ds.alg.meta:state-result ((,function-object ,function-class)
                                               ,state)
         (symbol-macrolet ,(mapcar
                            (lambda (i x) `(,x (aref ,state ,i)))
                            (iota length)
                            function-state)
           ,@result-form)))))


(defmacro define-aggregation-function
    (function-name function-class
     generic-lambda-list method-lambda-list
     function-state init-form
     aggregate-form result-form)
  (assert (find :range generic-lambda-list))
  (assert (find :range method-lambda-list))
  (setf generic-lambda-list (substitute 'range :range generic-lambda-list)
        method-lambda-list (substitute 'range :range method-lambda-list))
  (bind ((parsed-lambda-list (multiple-value-call #'vect (parse-ordinary-lambda-list method-lambda-list)))
         (rest-argument (aref parsed-lambda-list 2))
         ((:values required-arguments optional-arguments) (extract-parameters-as-key-arguments parsed-lambda-list)))
    `(progn
       ,(aggregation-function-class-form function-class)
       ,(aggregation-function-defgeneric-form function-name
                                              function-class
                                              required-arguments
                                              optional-arguments
                                              generic-lambda-list
                                              method-lambda-list
                                              rest-argument)
       ,(aggregation-function-make-state-form function-class
                                              function-state
                                              init-form)
       ,(aggregation-function-aggregate-form function-class
                                             function-state
                                             aggregate-form)
       ,(aggregation-function-result-form function-class
                                          function-state
                                          result-form))))
