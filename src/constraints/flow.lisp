(in-package #:cl-data-structures.constraints)


(defmacro with-operations (&body body)
  (with-gensyms (!block)
    `(block ,!block
       (macrolet ((reject (value)
                    (once-only (value)
                      `(if (typep ,value 'input)
                           (return-from ,',!block (monadic-value nil ,value))
                           (error "Can't reject something that is not input."))))
                  (accept (&rest result)
                    `(return-from ,',!block (monadic-value (list ,@result)
                                                           nil))))
         (list (progn ,@body)
               nil)))))


(defmacro defmon (name constructor-arguments constructor-form
                  arguments &body body)
  `(defun ,name ,constructor-arguments
     (lambda ()
       ,constructor-form
       (lambda ,arguments
         (with-operations
           ,@body)))))


(defmacro body (&body body)
  `(lambda ()
     (lambda (&rest args)
       (declare (ignore args))
       (with-operations
         ,@body))))


(defmon .or
    (&rest monads)
    (setf monads (mapcar #'funcall monads))
    (&rest all)
  (iterate
    (with result = nil)
    (with rejection-list = nil)
    (for monad in monads)
    (for (vals rejected) = (apply monad all))
    (when (not rejected)
      (setf result (cl-ds.utils:add-to-list result vals)))
    (when rejected
      (setf rejection-list (cl-ds.utils:add-to-list rejection-list
                                                    rejected)))
    (finally (return (if (endp result)
                         (rejected-monadic-value (first rejection-list))
                         (accepted-monadic-value result))))))


(defmon .and
    (&rest monads)
    (setf monads (mapcar #'funcall monads))
    (&rest all)
  (iterate
    (with result = nil)
    (for monad in monads)
    (for (vals rejected) = (apply monad all))
    (when rejected
      (leave (rejected-monadic-value rejected)))
    (setf result (cl-ds.utils:add-to-list result vals))
    (finally (return (accepted-monadic-value result)))))

