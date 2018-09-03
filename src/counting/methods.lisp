(in-package #:cl-data-structures.counting)


(defmethod type-count ((index set-index))
  (~> index access-reverse-mapping length))


(defmethod type-count ((node set-index-node))
  (iterate
    (for i from 0)
    (for n
         initially node
         then (read-parent n))
    (for type = (read-type n))
    (while type)
    (finally (return i))))


(defmethod type-count ((set set-in-index))
  (type-count (read-node set)))


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
    (error 'cl-ds:invalid-argument
           :argument 'apriori
           :references '((:apriori find-association))
           :text "Empty apriori list."))
  (when (emptyp aposteriori)
    (error 'cl-ds:invalid-argument
           :references '((:aposteriori find-association))
           :argument 'aposteriori
           :text "Empty aposteriori list."))
  (let* ((aposteriori (~> (add-to-list apriori aposteriori)
                          (remove-duplicates :test #'equal)))
         (node (apply #'node-at-names index aposteriori))
         (set-index-node (apply #'node-at-names index apriori)))
    (if (null set-index-node)
        (make 'empty-association-set :index index
                                     :type-count (length aposteriori))
        (make 'association-set
              :apriori-node set-index-node
              :node node
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
           :text "MINIMAL-FREQUENCY is supposed to be between 0 and 1."))
  (when (and maximal-size (<= maximal-size 0))
    (error 'cl-ds:argument-out-of-bounds
           :argument 'maximal-size
           :bounds '(> 0)
           :value maximal-size
           :text "MAXIMAL-SIZE is supposed to be above 0."))
  (data-range index
              minimal-frequency
              (lambda (x)
                (bind (((node _ . _) x))
                  (make-instance 'set-in-index
                                 :index index
                                 :node node
                                 :path (coerce (chain-cells x) 'vector))))
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
           :text "MINIMAL-FREQUENCY is supposed to be between 0 and 1."))
  (when (and maximal-size (<= maximal-size 0))
    (error 'cl-ds:argument-out-of-bounds
           :argument 'maximal-size
           :bounds '(> 0)
           :value maximal-size
           :text "MAXIMAL-SIZE is supposed to be above 0.")))


(defmethod all-super-sets ((set empty-mixin) minimal-frequency
                           &optional maximal-size)
  (all-sets (read-index set) minimal-frequency
            maximal-size))


(defmethod all-super-sets ((set set-in-index) minimal-frequency
                           &optional maximal-size)
  (nest
   (bind ((index (read-index set))
          (node (read-node set))))
   (cl-ds:xpr (:stack (list (list (chain-node node)
                                  (read-root index)
                                  (type-count set)))))
   (when-let ((cell (pop stack))))
   (bind (((chain parent depth) cell)
          (front (first chain))
          (content (read-sets parent)))
     (when (and maximal-size (> depth maximal-size))
       (recur :stack stack))
     (when (null front)
       (send-recur (make 'set-in-index :index index :node parent)
                   :stack (iterate
                            (for i from 0 below (length content))
                            (push (list (rest chain)
                                        (aref content i)
                                        (1+ depth))
                                  stack)
                            (finally (return stack)))))
     (let ((position (lower-bound content
                                  (read-type front)
                                  #'<
                                  :key #'read-type)))
       (when (= position (length content))
         (recur :stack stack))
       (when (eql (read-type front)
                  (~> content (aref position) read-type))
         (push (list (rest chain)
                     (aref content position)
                     (1+ depth))
               stack))
       (iterate
         (for i from 0 below position)
         (push (list chain (aref content i) (1+ depth)) stack))
       (recur :stack stack))
      (assert nil))))


(defmethod content ((set set-in-index))
  (when-let ((node (read-node set)))
    (~>> node chain-node
         (mapcar (curry #'node-name (read-index set))))))


(defmethod content ((set empty-mixin))
  nil)


(defmethod content ((set set-index))
  nil)


(defmethod aposteriori-set ((set association-set))
  (let ((types (~>> (just-post (read-apriori-node set) (read-node set))
                    (map 'list #'read-type))))
    (make 'set-in-index
          :node (apply #'node-at-type (read-index set) types)
          :index (read-index set))))


(defmethod apriori-set ((set association-set))
  (make 'set-in-index
        :node (read-apriori-node set)
        :index (read-index set)))


(defmethod make-association-set :before ((apriori set-in-index)
                                         (aposteriori set-in-index))
  (unless (eq (read-index apriori)
              (read-index aposteriori))
    (error 'cl-ds:operation-not-allowed
           :text "APRIORI and APOSTERIORI are not nested in the same index.")))


(defmethod apriori-set ((set empty-association-set))
  set)


(defmethod aposteriori-set ((set empty-association-set))
  set)


(defmethod make-association-set ((apriori set-index)
                                 (aposteriori set-in-index))
  aposteriori)


(defmethod make-association-set ((apriori set-in-index)
                                 (aposteriori set-in-index))
  (let* ((set-index-node (read-node apriori))
         (aposteriori-node (read-node aposteriori))
         (union (~>> (add-to-list (chain-node set-index-node)
                                  (chain-node aposteriori-node))
                     (mapcar #'read-type)
                     remove-duplicates
                     (apply #'node-at-type (read-index apriori)))))
    (or (and union
             (make 'association-set
                   :index (read-index apriori)
                   :node union
                   :apriori-node set-index-node))
        (make 'empty-association-set
              :index (read-index apriori)
              :type-count (type-count aposteriori-node)))))


(defmethod make-association-set ((apriori empty-mixin)
                                 (aposteriori empty-mixin))
  apriori)


(defmethod make-association-set ((apriori set-in-index)
                                 (aposteriori empty-mixin))
  (make 'association-set
        :index (read-index apriori)
        :apriori-node (read-node apriori)
        :node nil))


(defmethod make-association-set ((apriori empty-mixin)
                                 (aposteriori set-in-index))
  (make 'association-set
        :index (read-index apriori)
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
  (if-let ((node (apply #'node-at-names index content)))
    (make 'set-in-index
          :node node
          :index index)
    (make 'empty-set-in-index
          :index index
          :type-count (length content))))


(defmethod find-set ((set set-in-index) &rest content)
  (bind ((index (read-index set))
         (types1 (~>> set read-node chain-node
                      (mapcar #'read-type)))
         (types2 (~>> content validate-unique-names
                      (mapcar (curry #'name-to-type index))))
         (types (~> types1
                    (cl-ds.utils:add-to-list types2)
                    remove-duplicates)))
    (if-let ((node (and (every #'identity types)
                        (apply #'node-at-type index types))))
      (make 'set-in-index
            :node node
            :index index)
      (make 'empty-set-in-index
            :index index
            :type-count types))))


(defmethod cl-ds:at ((index set-index) location &rest more-locations)
  (let ((result (apply #'find-set index (cons location more-locations))))
    (if (typep result 'empty-set-in-index)
        (values nil nil)
        (values result t))))
