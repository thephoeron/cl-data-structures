(in-package #:cl-data-structures.counting)


(defmethod type-count ((index apriori-index))
  (~> index access-reverse-mapping length))


(defmethod type-count ((node apriori-node))
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
  0)


(defmethod association-frequency ((set apriori-set))
  (cond+ ((read-node set) (read-apriori-node set))
    ((t t) (/ (~> set read-node read-count)
              (~> set read-apriori-node read-count)))
    ((t nil) (/ (~> set read-node read-count)
                (~> set read-index read-total-size)))
    ((nil t) 1)
    ((nil nil) 1)))


(defmethod association-frequency ((set empty-apriori-set))
  0)


(defmethod find-association ((index apriori-index)
                             (apriori list)
                             (aposteriori list))
  (assert apriori)
  (assert aposteriori)
  (let ((aposteriori (~> (add-to-list apriori aposteriori)
                         (remove-duplicates :test #'equal))))
    (if-let ((aposteriori aposteriori)
             (node (apply #'node-at-names index aposteriori))
             (apriori-node (apply #'node-at-names index apriori)))
      (make 'apriori-set
            :apriori-node apriori-node
            :node node
            :index index)
      (make 'empty-apriori-set :index index))))


(defmethod all-sets ((index apriori-index) minimal-frequency
                     &optional maximal-size)
  (data-range index
              minimal-frequency
              (lambda (x)
                (make-instance 'set-in-index
                               :index index
                               :node x))
              maximal-size))


(defmethod all-super-sets ((set empty-mixin) minimal-frequency
                           &optional maximal-size)
  (all-sets (read-index set) minimal-frequency
            maximal-size))


(defmethod all-super-sets ((set set-in-index) minimal-frequency
                           &optional maximal-size)
  (bind ((index (read-index set))
         (node (read-node set)))
    (cl-ds:xpr (:stack (list (list (chain-node node)
                                   (read-root index)
                                   (type-count set))))
      (when-let ((cell (pop stack)))
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
            (if  (= position (length content))
                 (recur :stack stack)
                 (progn
                   (when (eql (read-type front)
                              (~> content (aref position) read-type))
                     (push (list (rest chain)
                                 (aref content position)
                                 (1+ depth))
                           stack))
                   (iterate
                     (for i from 0 below position)
                     (push (list chain (aref content i) (1+ depth)) stack))
                   (recur :stack stack))))
          (assert nil))))))


(defmethod content ((set set-in-index))
  (when-let ((node (read-node set)))
    (~>> node
         chain-node
         (mapcar (curry #'node-name (read-index set))))))


(defmethod content ((set empty-mixin))
  nil)


(defmethod aposteriori-set ((set apriori-set))
  (let ((types (~>> (just-post (read-apriori-node set) (read-node set))
                    (map 'list #'read-type))))
    (make 'set-in-index
          :node (apply #'node-at-type (read-index set) types)
          :index (read-index set))))


(defmethod apriori-set ((set apriori-set))
  (make 'set-in-index
        :node (read-apriori-node set)
        :index (read-index set)))


(defmethod apriori-set ((set empty-apriori-set))
  set)


(defmethod aposteriori-set ((set empty-apriori-set))
  set)


(defmethod make-apriori-set ((apriori set-in-index)
                             (aposteriori set-in-index))
  (assert (eq (read-index apriori) (read-index aposteriori)))
  (let* ((apriori-node (read-node apriori))
         (aposteriori-node (read-node aposteriori))
         (union (~>> (add-to-list (chain-node apriori-node)
                                  (chain-node aposteriori-node))
                    (mapcar #'read-type)
                    remove-duplicates
                    (apply #'node-at-type (read-index apriori)))))
    (or (and union
             (make 'apriori-set
                   :index (read-index apriori)
                   :node union
                   :apriori-node apriori-node))
        (make 'empty-apriori-set :index (read-index apriori)))))


(defmethod make-apriori-set ((apriori empty-mixin)
                             (aposteriori empty-mixin))
  apriori)


(defmethod make-apriori-set ((apriori set-in-index)
                             (aposteriori empty-mixin))
  (make 'apriori-set
        :index (read-index apriori)
        :apriori-node (read-node apriori)
        :node nil))


(defmethod make-apriori-set ((apriori empty-mixin)
                             (aposteriori set-in-index))
  (make 'apriori-set
        :index (read-index apriori)
        :apriori-node nil
        :node (read-node aposteriori)))


(defmethod support ((object empty-mixin))
  0)


(defmethod support ((object apriori-index))
  (read-total-size object))


(defmethod support ((object set-in-index))
  (support (read-node object)))


(defmethod support ((object apriori-node))
  (read-count object))


(defmethod find-set ((index apriori-index) &rest content)
  (if-let ((node (apply #'node-at-names index content)))
    (make 'set-in-index
          :node node
          :index index)
    (make 'empty-set-in-index :index index)))
