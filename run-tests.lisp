(ql:quickload :cl-data-structures)

(unwind-protect
     (let ((*error-output* (make-broadcast-stream)))
       (handler-bind
           ((lparallel.kernel:no-kernel-error
              (lambda (c)
                (declare (ignore c))
                (invoke-restart 'lparallel.kernel:make-kernel 4))))
         (prove:run :cl-data-structures
                    :reporter :dot)))
  (cl-user::quit))
