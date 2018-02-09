(in-package #:cl-data-structures.utils)


(defmacro lazy-let (bindings &body body)
  "Like LET but bind variable only once it is accessed. Does that by replacing calls to variable by calls to local function so variables are not seen when introspecting lexical enviorement."
  (flet ((gensym-list (x) (list (car x) (gensym) (cadr x) (caddr x) (gensym)))
         (make-let-list (x) `(,(cadr x) ',(cadr x)))
         (make-macro-list (x) (destructuring-bind (symbol var form rec init) x
                                (declare (ignore var form rec))
                                `(,symbol (,init))))
         (make-set-list (x) (destructuring-bind (symbol var form rec init) x
                              (declare (ignore form symbol rec))
                              `((setf ,init) (new-value)
                                (setf ,var new-value))))
         (make-init-list (x) (destructuring-bind (symbol var form rec init) x
                               `(,init ()
                                       ,(if rec
                                            `(progn (when (eq ,var ',var)
                                                      (let ((,symbol ,rec))
                                                        (setf ,var (nlet >>> (,symbol) ,form))))
                                                    ,var)
                                            `(progn (when (eq ,var ',var)
                                                      (setf ,var ,form))
                                                    ,var))))))
    (let* ((gensym-list (mapcar #'gensym-list bindings))
           (functions (mapcar (rcurry #'elt 4) gensym-list)))
      `(let ,(mapcar #'make-let-list gensym-list)
         (symbol-macrolet ,(mapcar #'make-macro-list gensym-list)
           (labels (,@(mapcar #'make-init-list gensym-list)
                    ,@(mapcar #'make-set-list gensym-list))
             (declare (ignorable ,@(mapcar (lambda (x) `(function ,x))
                                           functions)
                                 ,@(mapcar (lambda (x) `(function (setf ,x)))
                                           functions))
                      (inline ,@functions
                              ,@(mapcar (lambda (x) `(setf ,x))
                                        functions)))
             ,@body))))))


(defmacro bind-lambda (fn &rest args)
  "Curry function. For instance (bin-lambda #'+ 2 :_) is like (curry #+ 2). However this can be used to bind more arguments. For instance (bind-lambda #'/ 5 :_ 3 :_ 4 :_ 2 :_ :_ 12)"
  (let* ((args-count 0)
         (fargs nil)
         (binded nil)
         (funcall-args (mapcar (lambda (x)
                                 (if (eq :_ x)
                                     (let ((arg (intern (format nil "ARG~a" (incf args-count)))))
                                       (push arg fargs)
                                       arg)
                                     (let ((symbol (gensym)))
                                       (push (list symbol x) binded)
                                       symbol)))
                               args)))
    `(let ,(reverse binded)
       (lambda ,(reverse fargs)
         (funcall ,fn ,@funcall-args)))))


(defmacro with-vectors (vector-bindings &body body)
  "Macro for simpler vector usage. Will expand into local functions that act as accessors for vectors so you will not have to write stuff like (aref vector 0), instead you can write (vector 0)"
  (let ((vector-bindings (if (symbolp vector-bindings)
                             (list vector-bindings)
                             vector-bindings)))
    (with-gensyms (!index !value)
      (flet ((get-f-name (x)
               (if (symbolp x)
                   x
                   (car x)))
             (get-let-forms (x)
               (if (symbolp x)
                   (list x x)
                   x)))
        (flet ((make-aref-list (x)
                 `(,x (,!index) (aref ,x ,!index)))
               (make-setf-list (x)
                 `((setf ,x)  (,!value ,!index)
                   (setf (aref ,x ,!index) ,!value))))
          (let ((functions (mapcar #'get-f-name vector-bindings)))
            `(let* ,(mapcar #'get-let-forms vector-bindings)
               (labels (,@(mapcar #'make-aref-list functions)
                        ,@(mapcar #'make-setf-list functions))
                 (declare (ignorable ,@(mapcar (lambda (x) `(function ,x))
                                               functions)
                                     ,@(mapcar (lambda (x) `(function (setf ,x)))
                                               functions))
                          (dynamic-extent ,@(mapcar (lambda (x) `(function ,x))
                                                    functions)
                                          ,@(mapcar (lambda (x) `(function (setf ,x)))
                                                    functions))
                          (inline ,@functions
                                  ,@(mapcar (lambda (x) `(setf ,x))
                                            functions)))
                 ,@body))))))))


(eval-always
  (defun generate-if-else (conditions forms)
    (flet ((without-test (x)
             (destructuring-bind (tests form) x
               (list (cdr tests) form)))
           (check-test (x)
             (destructuring-bind ((b . w) form) x
               (declare (ignore w form))
               b)))
      (if conditions
          (list 'if (car conditions)
                (if-let ((r (mapcar #'without-test
                                    (remove-if (compose #'not #'check-test) forms))))
                  (generate-if-else (cdr conditions) r)
                  '(error "Unhalded case!"))
                (if-let ((r (mapcar #'without-test
                                    (remove-if #'check-test forms))))
                  (generate-if-else (cdr conditions) r)
                  '(error "Unhalded case!")))
          (cons 'progn (mapcar #'cadr forms)))))

  (defun every-possible-combination (count)
    (assert (>= count 0))
    (labels ((impl (count ac)
               (if (zerop count)
                   ac
                   (append (impl (1- count)
                                 (mapcar (curry #'cons t)
                                         ac))
                           (impl (1- count)
                                 (mapcar (curry #'cons nil)
                                         ac))))))
      (impl count '(nil)))))


(defmacro cond+ (tests &body forms)
  "If else ladder generator.

  @b(Values and arguments:)
  @begin(list)
  @item(tests -- list of conditions)
  @item(forms -- list of clauses that are supposed to be checked against conditions)
  @end(list)"
  (generate-if-else tests forms))


(defmacro cases (tests &body form)
  "Macro, used for elemination of code branches by duplicating code."
  (let* ((count (length tests)))
    `(cond+ ,tests
       ,@(mapcar (lambda (x) `(,x ,@form))
                 (every-possible-combination count)))))


(defmacro cond-compare ((a b) < = >)
  (once-only (a b)
    `(cond ((< ,a ,b) ,<)
           ((> ,a ,b) ,>)
           ((= ,a ,b) ,=))))


(defmacro let-functions (bindings &body body)
  `(let ,(mapcar (lambda (s)
                   (destructuring-bind (name default) s
                     `(,name (cond ((consp ,name)
                                    (let ((fname (second ,name)))
                                      `(function ,fname)))
                                   ((functionp ,name)
                                    `(function ,',default))
                                   (t ,name)))))
                 bindings)
     ,@body))


(defmacro inlined-funcall (fn &rest args)
  (if (consp fn)
      `(,(cadr fn) ,@args)
      `(funcall ,fn ,@args)))


(defmacro import-all-package-symbols (from-package to-package)
  "Macro. Will import all internal symbols from package to package. For tests."
  (let ((from-package (find-package from-package))
        (to-package (find-package to-package)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (do-symbols (symbol ,from-package)
         (when (nth-value 1 (find-symbol (string-upcase symbol) ,from-package))
           (import symbol ,to-package))))))


(eval-always
  (defun build-setf-form (vars args)
    (apply #'append
           (iterate
             (with p-fake-keys = (serapeum:plist-keys args))
             (with p-fake-values = (serapeum:plist-values args))
             (for key in p-fake-keys)
             (for value in p-fake-values)
             (for (real . fake) = (find (make-keyword key) vars
                                        :key #'car
                                        :test #'string=))
             (collect (list fake value))))))


(defmacro let-generator (forms &body body)
  "Poor man's generator (no continuations, no code-walking)."
  (with-gensyms (!end !result !self !finished)
    (let ((final-forms nil))
      (iterate
        (for (name vars . content) in forms)
        (for vars-length = (length vars))
        (for fake-vars = (iterate (repeat vars-length) (collect (gensym))))
        (for vars-asg = (mapcar #'list* vars fake-vars))
        (push `(,name (&key ,@vars)
                      (let ((,!finished nil))
                        (let ,(mapcar #'list fake-vars vars)
                          (lambda (&optional operation)
                            (if operation
                                (list ,@(apply #'append
                                               (mapcar (lambda (name binding) (list (make-keyword name) binding))
                                                       vars
                                                       fake-vars)))
                                (block ,!end
                                  (macrolet ((,(intern "SEND-FINISH") (&body ,!result)
                                               `(progn
                                                  (setf ,',!finished t)
                                                  (return-from ,',!end
                                                    (values (progn ,@,!result)
                                                            t))))
                                             (,(intern "FINISH") ()
                                               `(progn
                                                  (setf ,',!finished t)
                                                  (return-from ,',!end
                                                    (values nil nil))))
                                             (,(intern "RECUR") (&rest args)
                                               `(progn
                                                  (setf ,@(build-setf-form ',vars-asg args))
                                                  (go ,',!self)))
                                             (,(intern "SEND-RECUR") (,!result &rest args)
                                               `(return-from ,',!end
                                                  (values (prog1
                                                              ,,!result
                                                            (setf ,@(build-setf-form ',vars-asg args)))
                                                          t))))
                                    (tagbody
                                       ,!self
                                       (when ,!finished
                                         (return-from ,!end (values nil nil)))
                                       (let ,(mapcar #'list vars fake-vars)
                                         ,@content
                                         (setf ,!finished t)
                                         (return-from ,!end (values nil nil)))))))))))
              final-forms))
      `(flet ,final-forms
         ,@body))))
