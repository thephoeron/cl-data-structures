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
                    `(return-from ,',!block (monadic-value (list ,@result)))))
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
    (for monad in monads)
    (for (vals rejected) = (apply monad all))
    (when (not rejected)
      (leave (monadic-value vals)))
    (when rejected
      (collect rejected into rejection-list at start))
    (finally (return (apply #'monadic-value nil rejection-list)))))


(defmon .and
    (&rest monads)
    (setf monads (mapcar #'funcall monads))
    (&rest all)
  (iterate
    (for monad in monads)
    (for (vals rejected) = (apply monad all))
    (for p-vals previous vals)
    (when rejected
      (reject rejected))
    (finally (return (monadic-value p-vals)))))


(defmon .skip-rejected
    (&rest monads)
    (setf monads (mapcar #'funcall monads))
    (&rest all)
  (iterate
    (with accepted = nil)
    (with result = nil)
    (for monad in monads)
    (for (vals rejected) = (apply monad all))
    (when (not rejected)
      (setf accepted t)
      (setf result (cl-ds.utils:add-to-list result vals)))
    (collect rejected into rejection-list at start)
    (finally (if accepted
                 (return (monadic-value result))
                 (return (apply #'monadic-value nil rejection-list))))))
