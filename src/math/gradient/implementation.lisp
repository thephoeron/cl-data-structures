(in-package #:cl-data-structures.math.gradient)


(defun generate-weight-assigments (tape value-vector)
  (bind (((:slots %nodes %variables) tape)
         (result (make-hash-table :test 'equal)))
    (iterate
      (for node in-vector %nodes)
      (for deps = (tape-node-depends node))
      (for dependency-form = (iterate
                               (for dep in-vector deps)
                               (collect (aref value-vector dep))))
      (for symbols = (cl-ds.utils:list-of-unique-symbols (length deps)))
      (iterate
        (for dep in-vector deps)
        (for symbol in symbols)
        (setf (gethash (list node dep) result) symbol))
      (when (null (gethash (tape-node-symbol node) %variables))
        (collect (list symbols
                       (tape-backward-form (tape-node-symbol node)
                                           dependency-form))
          into forms))
      (finally
       (return (values result forms))))))


(defmethod compile-tape ((tape tape))
  (bind (((:slots %nodes %variables) tape)
         (length (length %nodes))
         (value-symbol-vector (coerce (cl-ds.utils:list-of-unique-symbols length)
                                      'vector))
         (reverse-value-symbol-vector (reverse value-symbol-vector))
         (total-dependency-count (reduce #'+ %nodes
                                         :key (compose #'length
                                                       #'tape-node-depends)
                                         :initial-value 0))
         ((:values node-weight-table weight-forms) (generate-weight-assigments
                                                    tape
                                                    value-symbol-vector))
         ((:flet index-to-value-symbol (indexes))
          (map 'list (curry #'elt value-symbol-vector) indexes))
         ((:flet pure-symbol (node))
          (emptyp (tape-node-depends node)))
         ((:flet initialization-form (node))
          (if (pure-symbol node)
              (tape-node-symbol node)
              (cons (tape-node-symbol node)
                    (index-to-value-symbol (tape-node-depends node)))))
         (result-symbol (elt value-symbol-vector (1- length)))
         (value-bindings (map 'list
                              (lambda (node symbol)
                                (list symbol
                                      (initialization-form node)))
                              %nodes
                              value-symbol-vector))
         (adjoints-vector-form `(make-array ,length
                                            :element-type 'double-float
                                            :initial-element 0.0d0))
         (!addjoints (gensym))
         (adjoint-init-form `(setf (aref ,!addjoints ,(1- length)) 1.0d0))
         (arguments (~> %variables
                        hash-table-keys
                        (sort #'string< :key #'symbol-name))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile nil
               `(lambda ,arguments
                  (declare (optimize (speed 3)))
                  (bind (,@value-bindings
                         (,!addjoints ,adjoints-vector-form)
                         ,@weight-forms)
                    ,adjoint-init-form
                    ,@(iterate
                        (for i from (1- length) downto 0)
                        (for node = (aref %nodes i))
                        (for depends = (tape-node-depends node))
                        (collect
                            (iterate
                              (for dependency in-vector depends)
                              (for weight-symbol = (gethash (list node dependency)
                                                            node-weight-table))
                              (collect `(incf (aref ,!addjoints ,dependency)
                                              (* ,weight-symbol
                                                 (aref ,!addjoints ,i)))))
                          into result)
                        (finally (return (apply #'append result))))
                    ,!addjoints))))))



(defun make-tape (expression)
  (bind ((nodes (make-array 8 :adjustable t
                              :element-type 'tape-node
                              :fill-pointer 0))
         (variables (make-hash-table))
         ((:labels impl (tree))
          (if (symbolp tree)
              (let ((variable-position (gethash tree variables)))
                (if (null variable-position)
                    (let ((position (length nodes)))
                      (setf (gethash tree variables) position)
                      (vector-push-extend (make-tape-node :symbol tree)
                                          nodes)
                      position)
                    variable-position))
              (iterate
                (with symbol = (first tree))
                (for deps in (rest tree))
                (for current-index = (impl deps))
                (collect current-index into result)
                (finally
                 (vector-push-extend (make-tape-node
                                      :symbol symbol
                                      :depends (coerce result '(vector fixnum))
                                      :weights (make-array (length result)
                                                           :element-type 'double-float
                                                           :initial-element 0.0d0))
                                     nodes)
                 (return (~> nodes length 1-)))))))
    (impl expression)
    (make 'tape :nodes nodes :variables variables)))


(defun evaluate-node (position node values)
  (bind ((dependency (tape-node-depends node))
         (weights (tape-node-weights node))
         (symbol (tape-node-symbol node))
         (arguments (map 'list (curry #'aref values) dependency))
         (value (apply #'tape-forward symbol arguments))
         (weight (apply #'tape-backward symbol arguments)))
    (setf (aref values position) value)
    (map-into weights #'identity weight)))


(defun gradient (tape &rest vals)
  (bind ((nodes (read-nodes tape))
         (variables (read-variables tape))
         (length (length nodes))
         (adjoints (make-array length
                               :element-type 'double-float
                               :initial-element 0.0d0))
         (values (make-array length
                             :element-type 'double-float
                             :initial-element 0.0d0)))
    (setf (aref adjoints (1- length)) 1.0d0)
    (iterate
      (for v
           initially vals
           then (cddr v))
      (until (endp v))
      (for symbol = (car v))
      (for value = (cadr v))
      (for index = (gethash symbol variables))
      (assert index)
      (setf (aref values index) value))
    (iterate
      (for i from 0 below length)
      (for node = (aref nodes i))
      (unless (~> node tape-node-depends emptyp)
        (evaluate-node i node values)))
    (iterate
      (for i from (1- length) downto 0)
      (for node = (aref nodes i))
      (for adjoint = (aref adjoints i))
      (for deps = (tape-node-depends node))
      (for weights = (tape-node-weights node))
      (iterate
        (for dependency in-vector deps)
        (for weight in-vector weights)
        (for change = (* weight adjoint))
        (incf (aref adjoints dependency) change)))
    adjoints))
