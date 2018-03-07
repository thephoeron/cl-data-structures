(in-package #:cl-data-structures.common.egnat)


(defun splice-content (data count)
  (let* ((size (cl-ds:size data))
         (content (unless (zerop count)
                    (cl-ds.alg:sequence-window data 0 count)))
         (new-data (unless (eql count (cl-ds:size data))
                     (cl-ds.alg:sequence-window data
                                                count
                                                (cl-ds:size data)))))
    (assert (> size count))
    (assert (eql count (cl-ds:size content)))
    (values content new-data)))


(defun select-seeds (branching-factor data-count)
  (iterate
    (with generator = (cl-ds.utils:lazy-shuffle 0 data-count))
    (with result = (make-array branching-factor
                               :element-type 'fixnum
                               :fill-pointer 0
                               :adjustable t))
    ;; map index of seed to index of children
    (with reverse-mapping = (make-hash-table :size branching-factor))
    (for i = (funcall generator))
    (repeat branching-factor)
    (while i)
    (vector-push-extend i result)
    (for j from 0)
    (setf (gethash i reverse-mapping) j)
    (finally (return (values result reverse-mapping)))))


(defun make-partitions (container seeds-indexes data)
  (let ((result (make-array (cl-ds:size data) :element-type 'fixnum))
        (index 0))
    (cl-ds:across (lambda (value)
                    (setf (aref result (finc index))
                          (iterate
                            (for seed-index in-vector seeds-indexes)
                            (for seed = (cl-ds:at data seed-index))
                            (for distance = (distance container value seed))
                            (minimize distance into min)
                            (for result
                                 initially 0
                                 then (if (= distance min)
                                          seed-index
                                          result))
                            (finally (return result)))))
                  data)
    result))


(defun make-contents (seeds-indexes data data-partitions reverse-mapping)
  (let ((result (make-array (length seeds-indexes))))
    (map-into result (compose #'vect (curry #'cl-ds:at data)) seeds-indexes)
    (iterate
      (for d in-vector data-partitions)
      (for i from 0)
      (for element = (cl-ds:at data i))
      (for partition = (gethash d reverse-mapping))
      ;; don't assign seeds to partitions because
      ;; it has been already done in map-into form
      (unless (eql d i)
        (vector-push-extend element (aref result partition))))
    result))


(defun make-ranges (container contents branching-factor)
  (bind ((range-size `(,branching-factor ,branching-factor))
         (metric-type (read-metric-type container))
         (close-range (make-array range-size
                                  :element-type metric-type))
         (distant-range (make-array range-size
                                    :element-type metric-type)))
    (iterate
      (for child from 0 below (length contents))
      (for head = (~> contents (aref child) (aref 0)))
      (iterate
        (for data from 0 below (length contents))
        (for init = (distance container head (~> contents
                                                 (aref data)
                                                 (aref 0))))
        (unless (eql child data)
          (multiple-value-bind (min max)
              (cl-ds.utils:optimize-value ((mini < init)
                                           (maxi < init))
                (map nil (lambda (x)
                           (let ((distance (distance container x head)))
                             (mini distance)
                             (maxi distance)))
                     (aref contents data)))
            (setf (aref close-range child data) min
                  (aref distant-range child data) max)))))
    (values close-range distant-range)))


(defun make-future-egnat-subtrees (container operation
                                   extra-arguments data)
  (bind (((:slots %content-count-in-node
                  %branching-factor)
          container)
         ((:values this-content this-data)
          (splice-content data %content-count-in-node))
         ((:values seeds-indexes reverse-mapping)
          (select-seeds %branching-factor (cl-ds:size this-data)))
         (data-partitions (make-partitions container
                                           seeds-indexes
                                           this-data))
         (contents (make-contents seeds-indexes this-data
                                  data-partitions reverse-mapping))
         (children (map 'vector
                        (lambda (content)
                          (lparallel:future
                            (make-future-egnat-nodes container
                                                     operation
                                                     extra-arguments
                                                     content)))
                        contents))
         ((:values close-range distant-range) (make-ranges container contents
                                                           %branching-factor))
         (content (make-array %content-count-in-node
                              :adjustable t
                              :fill-pointer 0)))
    (cl-ds:across (lambda (x)
                    (vector-push-extend (apply #'cl-ds:make-bucket
                                               operation
                                               container
                                               x
                                               extra-arguments)
                                        content))
                  this-content)
    (assert (= (reduce #'+ contents :key #'length)
               (cl-ds:size this-data)))
    (make 'egnat-node
          :content content
          :close-range close-range
          :distant-range distant-range
          :children children)))


(defun make-future-egnat-nodes (container operation
                                extra-arguments data)
  (if (<= (cl-ds:size data) (read-content-count-in-node container))
      (let ((content (make-array (read-content-count-in-node container)
                                 :adjustable t
                                 :fill-pointer 0)))
        (cl-ds:across (lambda (x)
                        (vector-push-extend (apply #'cl-ds:make-bucket
                                                   operation
                                                   container
                                                   x
                                                   extra-arguments)
                                            content))
                      data)
        (make 'egnat-node :content content))
      (make-future-egnat-subtrees container operation
                                  extra-arguments data)))


(defun make-egnat-tree (container operation extra-arguments data)
  (let* ((sync-thread (cl-ds.utils:make-pipe-fragment
                       (lambda (node sync-thread
                           &aux (children (read-children node)))
                         (map-into children #'lparallel:force children)
                         (map nil sync-thread children))
                       :end-on-empty t))
         (root (make-future-egnat-nodes container
                                        operation
                                        extra-arguments
                                        data)))
    (funcall sync-thread root)
    (cl-ds.utils:start-execution sync-thread)
    (cl-ds.utils:end-execution sync-thread)
    root))


(defun prune-subtrees (container trees
                       close-range distant-range
                       value margin
                       &optional (result (make-array (length trees)
                                                     :element-type 'bit
                                                     :initial-element 1)))
  (let ((length (length trees)))
    (iterate
      (for i from 0)
      (for tree in-vector trees)
      (for head = (~> tree read-content (aref 0)))
      (for distance = (distance container value head))
      (iterate
        (for j from 0 below length)
        (unless (or (eql i j) (zerop (aref result j)))
          (let ((in-range (<= (- (aref close-range i j)
                                 margin)
                              distance
                              (+ (aref distant-range i j)
                                 margin))))
            (setf (aref result j) (if in-range 1 0))))))
    result))


(defun insert-into-node (operation container node item extra-arguments)
  (bind (((:slots %content) node))
    (apply #'cl-ds:grow-bucket!
           operation
           container
           node
           item
           extra-arguments)))


(defun node-full (container node)
  (cl-ds:full-bucket-p container (read-content node)))


(defun closest-node (container nodes item)
  (iterate
    (for node in-vector nodes)
    (for content = (read-content node))
    (for head = (aref content 0))
    (for distance = (distance container head item))
    (minimize distance into min)
    (for result
         initially node
         then (if (= distance min)
                  node
                  result))
    (finally (return result))))


(defun traverse-impl (container function)
  (labels ((impl (tree)
             (let ((content (read-content tree)))
               (when content
                 (map nil function content)
                 (map nil #'impl (read-children tree))))))
    (impl (access-root container)))
  container)


(defun egnat-destructive-grow (container item)
  (bind (((:slots %metric-f %same-fn %content-count-in-node
                  %size %root %branching-factor)
          container))
    (labels ((ímpl (node)
               (let* ((children (read-children node))
                      (content (read-content node))
                      (length (length content)))
                 (cond ((position item content
                                  :test (curry #'same container))
                        cl-ds.utils:todo)
                       ((eql length %content-count-in-node)
                        (if (null children)
                            cl-ds.utils:todo
                            cl-ds.utils:todo))
                       ;; actually, we need to check if item is not in the
                       ;; tree already. This whole function needs to be
                       ;; different and some sort of linear scan should be
                       ;; performed. Perhaps I can use already existing
                       ;; cl-ds:near to get range around item and then,
                       ;; slowly build all possible paths of nodes,
                       ;; that may lead to it. Finally, item should be
                       ;; inserted into first node that has place for it,
                       ;; or new node should be created if no node like,
                       ;; this exists.
                       (t (let ((closest-node cl-ds.utils:todo))
                            (impl closest-node)))))))
      (impl %root))))
