(in-package #:cl-data-structures.counting)


(defmethod type-count ((index set-index))
  (~> index access-reverse-mapping length))


(defmethod type-count ((set set-in-index))
  (length (read-path set)))


(defmethod type-count ((set empty-mixin))
  (read-type-count set))


(defmethod association-frequency ((set association-set))
  (cond+ ((read-node set) (read-apriori-node set))
    ((t t) (/ (~> set read-node read-count)
              (~> set read-apriori-node read-count)))
    ((t nil) (/ (~> set read-node read-count)
                (~> set read-index read-total-size)))
    ((nil t) 1)
    ((nil nil) 1)))


(defmethod association-frequency ((set empty-association-set))
  0)


(defmethod find-association ((index set-index)
                             (apriori list)
                             (aposteriori list))
  (when (emptyp apriori)
    (error 'cl-ds:invalid-value
           :value '()
           :references '((:apriori find-association))
           :format-control "Empty apriori list."))
  (when (emptyp aposteriori)
    (error 'cl-ds:invalid-value
           :references '((:aposteriori find-association))
           :value '()
           :format-control "Empty aposteriori list."))
  (bind ((aposteriori (~> (add-to-list apriori aposteriori)
                          (remove-duplicates :test #'equal)))
         ((:values node path) (node-at-names index aposteriori))
         ((:values set-index-node apriori-path) (node-at-names index apriori)))
    (if (or (null node) (null set-index-node))
        (make 'empty-association-set :index index
                                     :type-count (length aposteriori))
        (make 'association-set
              :apriori-node set-index-node
              :node node
              :path path
              :apriori-path apriori-path
              :index index))))


(defmethod all-sets ((index set-index) minimal-frequency
                     &optional maximal-size)
  (check-type minimal-frequency real)
  (check-type maximal-size (or null integer))
  (unless (<= 0 minimal-frequency 1)
    (error 'cl-ds:argument-out-of-bounds
           :argument 'minimal-frequency
           :bounds '(<= 0 1)
           :value minimal-frequency
           :format-control "MINIMAL-FREQUENCY is supposed to be between 0 and 1."))
  (when (and maximal-size (<= maximal-size 0))
    (error 'cl-ds:argument-out-of-bounds
           :argument 'maximal-size
           :bounds '(> 0)
           :value maximal-size
           :format-control "MAXIMAL-SIZE is supposed to be above 0."))
  (data-range index
              minimal-frequency
              (lambda (x)
                (bind (((node _ . _) x))
                  (make-instance 'set-in-index
                                 :index index
                                 :node node
                                 :path (~> x chain-cells
                                           rest (coerce 'vector)))))
              maximal-size))


(defmethod all-super-sets :before (set minimal-frequency
                                   &optional maximal-size)
  (check-type minimal-frequency real)
  (check-type maximal-size (or null integer))
  (unless (<= 0 minimal-frequency 1)
    (error 'cl-ds:argument-out-of-bounds
           :argument 'minimal-frequency
           :bounds '(<= 0 1)
           :value minimal-frequency
           :format-control "MINIMAL-FREQUENCY is supposed to be between 0 and 1."))
  (when (and maximal-size (<= maximal-size 0))
    (error 'cl-ds:argument-out-of-bounds
           :argument 'maximal-size
           :bounds '(> 0)
           :value maximal-size
           :format-control "MAXIMAL-SIZE is supposed to be above 0.")))


(defmethod all-super-sets ((set empty-mixin) minimal-frequency
                           &optional maximal-size)
  (all-sets (read-index set) minimal-frequency
            maximal-size))


(defmethod read-sets ((node set-index-node))
  #())


(defmethod all-super-sets ((set set-in-index) minimal-frequency
                           &optional maximal-size)
  (nest
   (bind ((index (read-index set))
          (node (read-node set))
          (chain (coerce (read-path set) 'list))))
   (cl-ds:xpr (:stack (list (list chain
                                  (read-root index)
                                  0
                                  nil))))
   (when-let ((cell (pop stack))))
   (bind (((chain parent depth prev) cell)
          (front (first chain))
          (content (read-sets parent)))
     (when (and maximal-size (> depth maximal-size))
       (cl-ds:recur :stack stack))
     (when (null front)
       (cl-ds:send-recur (make 'set-in-index
                               :index index
                               :node parent
                               :path (iterate
                                       (with path = (vect))
                                       (for (chain node depth parent)
                                            initially cell
                                            then parent)
                                       (until (null parent))
                                       (vector-push-extend node path)
                                       (finally (return path))))
                         :stack (iterate
                                  (for i from 0 below (length content))
                                  (push (list (rest chain)
                                              (aref content i)
                                              (1+ depth)
                                              cell)
                                        stack)
                                  (finally (return stack)))))
     (let ((position (lower-bound content
                                  (read-type front)
                                  #'<
                                  :key #'read-type)))
       (when (= position (length content))
         (cl-ds:recur :stack stack))
       (when (eql (read-type front)
                  (~> content (aref position) read-type))
         (push (list (rest chain)
                     (aref content position)
                     (1+ depth)
                     cell)
               stack))
       (iterate
         (for i from 0 below position)
         (push (list chain (aref content i) (1+ depth) cell) stack))
       (cl-ds:recur :stack stack))
     (assert nil))))


(defmethod content ((set set-in-index))
  (when-let ((node (read-node set)))
    (~>> set read-path
         (remove nil _ :key #'read-type)
         (map 'list (curry #'node-name (read-index set))))))


(defmethod content ((set empty-mixin))
  nil)


(defmethod content ((set set-index))
  nil)


(defmethod aposteriori-set ((set association-set))
  (bind ((apriori (read-apriori-node set))
         (aposteriori (read-node set))
         (types (and apriori
                     aposteriori
                     (~>> (cl-ds.utils:ordered-exclusion
                           (lambda (a b) (< (read-type a) (read-type b)))
                           (lambda (a b) (eql (read-type a) (read-type b)))
                           (read-path set)
                           (read-apriori-path set))
                          (map 'list #'read-type))))
         ((:values result-node path) (node-at-type (read-index set) types)))
    (make 'set-in-index
          :node result-node
          :path path
          :index (read-index set))))


(defmethod apriori-set ((set association-set))
  (make 'set-in-index
        :node (read-apriori-node set)
        :path (read-apriori-path set)
        :index (read-index set)))


(defmethod make-association-set :before ((apriori set-in-index)
                                         (aposteriori set-in-index))
  (unless (eq (read-index apriori)
              (read-index aposteriori))
    (error 'cl-ds:operation-not-allowed
           :format-control "APRIORI and APOSTERIORI are not nested in the same index.")))


(defmethod apriori-set ((set empty-association-set))
  set)


(defmethod aposteriori-set ((set empty-association-set))
  set)


(defmethod make-association-set ((apriori set-index)
                                 (aposteriori set-in-index))
  aposteriori)


(defmethod make-association-set ((apriori set-in-index)
                                 (aposteriori set-in-index))
  (bind ((effective-path (~> (concatenate 'vector
                                          (read-path apriori)
                                          (read-path aposteriori))
                             (remove-duplicates :key #'read-type)))
         ((:values union aposteriori-path)
          (~>> effective-path
               (map 'vector #'read-type)
               (node-at-type (read-index apriori)))))
    (or (and union
             (make 'association-set
                   :index (read-index apriori)
                   :path aposteriori-path
                   :apriori-path (read-path apriori)
                   :node union
                   :apriori-node (read-node apriori)))
        (make 'empty-association-set
              :index (read-index apriori)
              :type-count (length effective-path)))))


(defmethod make-association-set ((apriori empty-mixin)
                                 (aposteriori empty-mixin))
  apriori)


(defmethod make-association-set ((apriori set-in-index)
                                 (aposteriori empty-mixin))
  (make 'association-set
        :index (read-index apriori)
        :path (read-path apriori)
        :apriori-node (read-node apriori)
        :node nil))


(defmethod make-association-set ((apriori empty-mixin)
                                 (aposteriori set-in-index))
  (make 'association-set
        :index (read-index apriori)
        :path (read-path aposteriori)
        :apriori-node nil
        :node (read-node aposteriori)))


(defmethod support ((object empty-mixin))
  0)


(defmethod support ((object set-index))
  (read-total-size object))


(defmethod support ((object set-in-index))
  (support (read-node object)))


(defmethod support ((object set-index-node))
  (read-count object))


(defmethod find-set ((index set-index) &rest content)
  (bind (((:values node path) (node-at-names index content)))
    (if node
        (make 'set-in-index
              :node node
              :path path
              :index index)
        (make 'empty-set-in-index
              :index index
              :type-count (length content)))))


(defmethod find-set ((set set-in-index) &rest content)
  (bind ((index (read-index set))
         (types1 (~>> set read-path (mapcar #'read-type)))
         (types2 (~>> content validate-unique-names
                      (mapcar (curry #'name-to-type index))))
         (types (~> types1
                    (cl-ds.utils:add-to-list types2)
                    remove-duplicates)))
    (bind (((:values node path) (and (every #'identity types)
                                     (apply #'node-at-type index types))))
      (if node
          (make 'set-in-index
                :node node
                :path path
                :index index)
          (make 'empty-set-in-index
                :index index
                :type-count types)))))


(defmethod cl-ds:at ((index set-index) location &rest more-locations)
  (let ((result (apply #'find-set index (cons location more-locations))))
    (if (typep result 'empty-set-in-index)
        (values nil nil)
        (values result t))))
