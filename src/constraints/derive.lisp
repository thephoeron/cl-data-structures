(in-package #:cl-data-structures.constraints)


(defvar *depth* 0)


(defmacro derive (bindings body)
  (with-gensyms (!depth !next !this)
    (let ((all-symbols (mapcar #'first bindings)))
      `(lambda ()
         (let* (,@all-symbols
                (,!next (let ((*depth* (1+ *depth*)))
                          (funcall ,body)))
                (,!this (lambda ()
                          (if (some #'reached-end '(,@all-symbols))
                              (values nil nil)
                              (funcall ,!next)))))
           ,(initialize-bindings-form bindings !this !depth)
           ,!this)))))
