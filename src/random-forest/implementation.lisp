(cl:in-package #:cl-ds.rf)


(defmethod make-submodel ((main-model random-forest-classifier)
                          creation-context
                          data)
  (make-submodel-of-class (submodel-class main-model)
                          creation-context
                          data))


(defmethod make-submodel-creation-context ((main-model random-forest-classifier))
  (make-submodel-creation-context-of-class (submodel-class main-model)))


(defmethod make-model ((class (eql 'random-forest-classifier))
                       data arguments)
  (bind ((result (apply #'make 'random-forest-classifier
                        arguments))
         (tree-count (tree-count result))
         (trees (make-array tree-count)))
    (setf (access-submodels result) trees)
    (iterate
      (for i from 0 below tree-count)
      (setf (aref trees i) (make-node result data)))
    result))


(defmethod make-node ((main-model random-forest-classifier)
                      data)
  (let ((split-attempts (split-attempts main-model))
        (submodel-class (submodel-class main-model))
        (tree-maximum-depth (tree-maximum-depth main-model))
        (tree-minimal-size (tree-minimal-size main-model)))
    (labels ((summary-group-to-node (content depth group-class
                                     creation-context)
               (let ((size (length content)))
                 (if (and (>= size tree-minimal-size)
                          (> depth 0))
                     (node (1- depth) content group-class
                           creation-context)
                     (make-instance
                      'leaf-node
                      :label group-class
                      :class (~> content
                                 (cl-ds.alg:group-by :key #'second)
                                 cl-ds.alg:count-elements
                                 (cl-ds.alg:extremum (lambda (a b)
                                                       (> (cdr a) (cdr b))))
                                 car)))))
             (gini-impurity (summary data-size)
               (~> summary
                   (cl-ds.alg:accumulate
                    #'+
                    :initial-value 0.0
                    :key (lambda (group &aux (x (cdr group)))
                           (* (/ (cl-ds:at x :count)
                                 data-size)
                              (cl-ds:at x :gini))))))
             (node (depth data class creation-context)
               (iterate outer
                 (with contexts = (make-submodel-prediction-contexts main-model 1))
                 (with context = (first-elt contexts))
                 (repeat split-attempts)
                 (for clone = (cl-ds:clone creation-context))
                 (for submodel = (make-submodel main-model clone data))
                 (for submodel-predictions =
                      (map 'vector
                           (lambda (data)
                             (encode-data-into-contexts-of-class submodel-class
                                                                 contexts
                                                                 (first data))
                             (list data (submodel-predict submodel context)))
                           data))
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
                 (finding (list summary submodel clone)
                          minimizing gini-impurity
                          into result)
                 (finally
                  (iterate
                    (with (summary submodel context) = result)
                    (with size = (cl-ds:size summary))
                    (with children = (make-array size))
                    (for i from 0 below size)
                    (for (values data more) = (cl-ds:consume-front summary))
                    (while more)
                    (for (group-class . group) = data)
                    (for content = (cl-ds:at group :content))
                    (setf (aref children i)
                          (summary-group-to-node content depth
                                                 group-class context))
                    (finally (return-from outer
                               (make 'subtree-node
                                     :label class
                                     :submodel submodel
                                     :children children))))))))
      (node tree-maximum-depth data nil
            (make-submodel-creation-context main-model)))))


(defun prediction-in-tree (node context)
  (if (typep node 'leaf-node)
      (access-class node)
      (let* ((submodel (read-submodel node))
             (children (read-children node))
             (prediction (submodel-predict submodel context))
             (child (find prediction children :key #'read-label)))
        (prediction-in-tree child context))))


(defmethod make-submodel-prediction-contexts ((model random-forest-classifier)
                                              count)
  (check-type count positive-fixnum)
  (make-submodel-prediction-contexts-of-class (submodel-class model) count))


(defun elect-result (predictions)
  (~> predictions
      cl-ds.alg:count-elements
      (cl-ds.alg:extremum (lambda (a b) (> (cdr a) (cdr b))))
      car))


(defun prediction-function (model)
  (let* ((trees (access-submodels model))
         (submodel-counts (length trees))
         (contexts (make-submodel-prediction-contexts model submodel-counts))
         (submodel-class (submodel-class model))
         (submodel-predictions (~> trees length make-array))
         (grouped-predictions (cl-ds.alg:group-by submodel-predictions)))
    (declare (type vector contexts)
             (type (simple-array t (*)) submodel-predictions))
    (lambda (input)
      (encode-data-into-contexts-of-class submodel-class contexts input)
      (map-into submodel-predictions
                #'prediction-in-tree
                trees
                contexts)
      (elect-result grouped-predictions))))


(defmethod predict ((model random-forest-classifier) data)
  (~>> model prediction-function (cl-ds.alg:on-each data)))

(defmethod encode-data-into-contexts ((model random-forest-classifier)
                                      contexts
                                      data)
  (encode-data-into-contexts-of-class (submodel-class model)
                                      contexts
                                      data))

(defmethod encode-data-into-contexts-of-class ((class t) submodels
                                               contexts data)
  (iterate
    (for context in-vector contexts)
    (for node in-vector submodels)
    (for submodel = (read-submodel node))
    (encode-data-into-context submodel context data))
  contexts)
