(in-package #:cl-data-structures.constraints)


(defvar *depth* 0)


(eval-always
  (defun construct-binding-form (name deps form)
    `(,name (make-input '(,@deps)
                        *depth*
                        (lambda (,@deps)
                          ,form))))

  (defun construct-bindings-form (bindings)
    (declare (type list bindings))
    (cons 'setf
          (apply #'append
                 (iterate
                   (for (name deps form) in bindings)
                   (check-type name symbol)
                   (check-type deps list)
                   (collect (if (null deps)
                                `(,name ,form)
                                (construct-binding-form name deps
                                                        form))))))))


(defmacro derive (bindings body)
  (with-gensyms (!next !this !depth)
    (let ((all-symbols (mapcar #'first bindings)))
      `(lambda ()
         (let* (,@all-symbols
                (,!next (let ((*depth* (1+ *depth*)))
                          (funcall ,body)))
                (,!depth *depth*)
                (,!this (lambda ()
                          (tagbody ,!this
                             (if (or ,@(mapcar (lambda (x) `(reached-end ,x))
                                               all-symbols))
                                 (values nil nil)
                                 (bind ((values-rejected (funcall ,!next))
                                        ((values rejected) values-status))
                                   (if (or (null rejected)
                                           (not (same-depth rejected ,!depth)))
                                       values-rejected
                                       (progn
                                         (cl-ds:consume-front (read-range rejected))
                                         (go ,!this)))))))))
           ,(construct-bindings-form bindings)
           ,!this)))))
