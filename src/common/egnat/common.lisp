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
                                           (maxi > init))
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


(defun find-destination-node (container item)
  (let* ((root (access-root container))
         (stack (list (cons root 0)))
         (range (make 'egnat-grow-range
                      :near item
                      :stack stack
                      :initial-stack stack
                      :container container))
         (existing-item (iterate
                          (for (values item more) =
                               (cl-ds:consume-front range))
                          (while more)
                          (finding item such-that item))))
    (values (read-possible-paths range)
            (if (null existing-item) nil t)
            (access-last-node range))))


(defun egnat-destructive-grow (container item operation additional-arguments)
  (bind (((:slots %metric-f %same-fn %content-count-in-node
                  %size %root %branching-factor)
          container)
         ((:values paths found last-node) (find-destination-node container
                                                                 item)))
    #|
    following cases needs to be considered:
    1) item already present in the egnat. Simply attempt to change bucket, and roll with result
    2) item not present in the egnat, but there is a node that we can stick it in, without updating ranges. Do it and be happy.
    3) item not present in the egnat, and every node in path is already full. Find first node (from the root) that can hold another children.
       Create node by spliting content of parent in two. Update ranges of parent since new children has been added. Then update ranges of whole path
       because new item has been added.
    |#
    (if found ; case number 1, easy to handle
        (bind ((content (read-content last-node))
               (position (position item content :test (curry #'same container)))
               (bucket (aref content position))
               ((:values new-bucket status changed) (apply #'cl-ds:grow-bucket!
                                                           operation
                                                           container
                                                           bucket
                                                           item
                                                           additional-arguments)))
          (when changed
            (setf (aref content position) new-bucket))
          (values container status))
        (iterate
          (for (node index) in-hashtable paths)
          (for content = (read-content node))
          (finding node
                   such-that (< (fill-pointer content) %content-count-in-node)
                   into result)
          (finally
           ;; checking if it is the case number 2
           (if (null result)
               cl-ds.utils:todo ; case 3, it will be messy...
               ;; the case number 2, just one push-extend and we are done
               (bind ((content (read-content result))
                      ((:values new-bucket status)
                       (apply #'cl-ds:make-bucket
                              operation
                              container
                              item
                              additional-arguments)))
                 (vector-push-extend new-bucket content)
                 (return (values container status)))))))
    ))



(defun walk-path (fn node possible-paths)
  (iterate
    (for p
         initially (cons node 0)
         then (gethash (car p) possible-paths))
    (until (null p))
    (funcall fn p)))
