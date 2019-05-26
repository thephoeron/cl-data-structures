(cl:in-package #:cl-ds.rf)


(defmethod make-submodel ((main-model random-forest-classifier)
                          data
                          creation-context)
  (make-submodel (submodel-class main-model)
                 data
                 (submodel-arguments main-model)
                 creation-context))


(defmethod make-submodel-creation-context ((main-model random-forest-classifier))
  (make-submodel-creation-context-of-class (submodel-class main-model)
                                           main-model))


(defmethod make-model ((class (eql 'random-forest-classifier))
                       data arguments submodel-arguments)
  (bind ((result (apply #'make 'random-forest-classifier
                        :submodel-arguments submodel-arguments
                        arguments))
         (tree-count (tree-count result))
         (trees (make-array tree-count)))
    (iterate
      (for i from 0 below tree-count)
      (setf (aref trees i) (make-node result data)))
    (setf (access-submodels result) trees)
    result))


(defmethod make-node ((main-model random-forest-classifier)
                      data)
  (let ((split-attempts (split-attempts main-model))
        (tree-maximum-depth (tree-maximum-depth main-model))
        (tree-minimal-size (tree-minimal-size main-model)))
    (labels ((summary-group-to-node (content depth group-class)
               (bind ((size (cl-ds:at content :count))
                      (group-data (cl-ds:at content :content))
                      (node (if (>= size tree-minimal-size)
                                (node (1- depth) group-data group-class)
                                (make-instance
                                 'leaf-node
                                 :class (~> content
                                            (cl-ds.alg:group-by :key #'second)
                                            cl-ds.alg:count-elements
                                            (cl-ds.alg:extremum #'> :key #'cdr)
                                            car)))))
                 node))
             (gini-impurity (summary data-size)
               (cl-ds.alg:accumulate summary
                                     #'+
                                     :initial-value 0.0
                                     :key (lambda (group &aux (x (cdr group)))
                                            (* (/ (cl-ds:at x :count)
                                                  data-size)
                                               (cl-ds:at x :gini)))))
             (node (data depth class)
               (when (<= depth 0)
                 (return-from node nil))
               (iterate outer
                 (repeat split-attempts)
                 (for submodel = (make-submodel-with-model main-model
                                                           data))
                 (train submodel data)
                 (for submodel-predictions = (predict main-model data))
                 (for data-size = (length data))
                 (for grouped-predictions = (cl-ds.alg:group-by
                                                submodel-predictions
                                              :key #'second))
                 (for summary = (cl-ds.alg:summary grouped-predictions
                                  :count (cl-ds.alg:count-elements)
                                  :content (cl-ds.alg:to-vector :key #'first)
                                  :gini (cl-ds.math:gini-impurity
                                         :key #'cadar)))
                 (for classes-count = (cl-ds:size summary))
                 (for gini-impurity = (gini-impurity summary
                                                     data-size))
                 (finding (list* summary submodel)
                          minimizing gini-impurity
                          into result)
                 (finally
                  (iterate
                    (with summary = (car result))
                    (with submodel = (cdr result))
                    (with size = (cl-ds:size summary))
                    (with children = (make-array size))
                    (for i from 0 below size)
                    (for (values data more) = (cl-ds:consume-front summary))
                    (while more)
                    (for (group-class . group) = data)
                    (for content = (cl-ds:at group :content))
                    (cl-ds.utils:transform #'first content)
                    (setf (aref children i)
                          (summary-group-to-node content depth group-class))
                    (finally (return-from outer
                               (make 'subtree-node
                                     :class class
                                     :submodel submodel
                                     :children children))))))))
      (node data tree-maximum-depth nil))))


(defun prediction-in-tree (input node context)
  (if (typep node 'leaf-node)
      (access-class node)
      (let* ((submodel (read-submodel node))
             (children (read-children node))
             (prediction (submodel-predict submodel input context))
             (child (find prediction children :key #'access-class)))
        (prediction-in-tree child context prediction))))


(defmethod make-submodel-prediction-contexts ((model random-forest-classifier)
                                              count)
  (check-type count positive-fixnum)
  (make-submodel-prediction-contexts-of-class (submodel-class model)
                                              model
                                              count))


(defun elect-result (predictions)
  (~> predictions
      cl-ds.alg:count-elements
      (cl-ds.alg:extremum #'> :key #'cdr)
      car))


(defun prediction-function (model)
  (let* ((trees (access-submodels model))
         (submodel-counts (length trees))
         (contexts (make-submodel-prediction-contexts model submodel-counts))
         (submodel-predictions (~> trees length
                                   (make-array :element-type 'fixnum)))
         (grouped-predictions (cl-ds.alg:group-by submodel-predictions)))
    (declare (type vector contexts)
             (type (simple-array fixnum (*)) submodel-predictions))
    (lambda (input)
      (map-into submodel-predictions
                (curry #'prediction-in-tree input)
                trees
                contexts)
      (elect-result grouped-predictions))))


(defmethod predict ((model random-forest-classifier) data)
  (~> model prediction-function (cl-ds.alg:on-each data)))


(defmethod encode-data-into-contexts ((model random-forest-classifier)
                                      contexts
                                      data)
  (iterate
    (for context in-vector contexts)
    (for node in-vector (access-submodels model))
    (for submodel = (read-submodel model))
    (encode-data-into-context submodel context data))
  contexts)
