(in-package #:cl-data-structures.constraints)


(defmacro derive (bindings body)
  (with-gensyms (!depth !next !this)
    (let ((all-symbols (mapcar #'first bindings)))
      `(lambda (&optional (depth 0))
         (let* (,@all-symbols
                (,!next (funcall ,body (1+ depth)))
                (,!depth depth)
                (,!this (lambda ()
                          (if (some #'reached-end '(,@all-symbols))
                              (values nil nil)
                              (funcall ,!next)))))
           ,(initialize-bindings-form bindings !this !depth)
           ,!this)))))
