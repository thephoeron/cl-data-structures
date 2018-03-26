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


(defun make-ranges (container contents)
  (bind ((branching-factor (read-branching-factor container))
         (range-size `(,branching-factor ,branching-factor))
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
         (children (map-into
                    (make-array (length contents)
                                :adjustable t
                                :fill-pointer (length contents))
                    (lambda (content)
                      (lparallel:future
                        (make-future-egnat-nodes container
                                                 operation
                                                 extra-arguments
                                                 content)))
                    contents))
         ((:values close-range distant-range) (make-ranges container contents))
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
  (let ((root (access-root container)))
    (if (null root)
        (values (make-hash-table :test 'eq)
                nil
                nil)
        (bind ((root (access-root container))
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
                  (access-last-node range))))))


(-> reinitialize-ranges! (mutable-egnat-container egnat-node)
    t)
(defun reinitialize-ranges! (container node)
  "Updates existing ranges for new node (last children)."
  (bind ((children (read-children node))
         (length (length children))
         (last (1- length))
         (new-node (aref children last))
         (item (~> new-node read-content (aref 0)))
         (close-range (read-close-range node))
         (distant-range (read-distant-range node)))
    (iterate
      (for i from 0 below last)
      (for (values mini maxi) =
           (cl-ds.utils:optimize-value ((mini <)
                                        (maxi >))
             (labels ((impl (node)
                        (iterate
                          (for content in-vector (read-content node))
                          (for distance = (distance container content item))
                          (mini distance)
                          (maxi distance))
                        (map nil #'impl (read-children node))))
               (declare (dynamic-extent #'impl))
               (impl (aref children i)))))
      (setf (aref distant-range last i) maxi
            (aref close-range last i) mini))))


(-> update-ranges! (mutable-egnat-container egnat-node t fixnum)
    mutable-egnat-container)
(defun update-ranges! (container node item closest-index)
  (let ((children (read-children node))
        (close-range (read-close-range node))
        (distant-range (read-distant-range node)))
    (iterate
      (for i from 0 below (length children))
      (for child = (aref children i))
      (unless (eql i closest-index)
        (let ((distance (~> child
                            read-content
                            (aref 0)
                            (distance container _ item))))
          (symbol-macrolet ((close (aref close-range i closest-index))
                            (distant (aref distant-range i closest-index)))
            (setf close (if (zerop close)
                            distance
                            (min close distance))
                  distant (max distant distance))))))
    container))


(-> initialize-root! (mutable-egnat-container cl-ds:grow-function t list)
    (values mutable-egnat-container
            cl-ds.common:eager-modification-operation-status))
(defun initialize-root! (container operation item additional-arguments)
  (bind (((:slots %root %branching-factor %metric-type %size) container)
         (range-size `(,%branching-factor ,%branching-factor))
         (close-range (make-array range-size
                                  :element-type %metric-type))
         (distant-range (make-array range-size
                                    :element-type %metric-type))
         (bucket (apply #'cl-ds:make-bucket
                        operation
                        container
                        item
                        additional-arguments)))
    (assert (null %root))
    (setf %root (make-instance 'egnat-node
                               :children (vect)
                               :content (vect bucket)
                               :close-range close-range
                               :distant-range distant-range)
          %size 1)
    (values container
            cl-ds.common:empty-eager-modification-operation-status)))


(defun push-children! (node child)
  (bind (((:slots %children) node))
    (if (null %children)
        (setf %children (vect child))
        (vector-push-extend child %children)))
  node)


(-> splitting-grow! (mutable-egnat-container cl-ds:grow-function t list)
    (values mutable-egnat-container
            cl-ds.common:eager-modification-operation-status))
(defun splitting-grow! (container operation item additional-arguments)
  (fbind ((distance (curry #'distance container)))
    (bind (((:slots %root %branching-factor %size %metric-type) container)
           ((:dflet closest-index (array))
            (let ((result 0))
              (cl-ds.utils:optimize-value ((mini <))
                (iterate
                  (for i from 0 below (length array))
                  (for child = (aref array i))
                  (for content = (read-content child))
                  (for head = (aref content 0))
                  (for distance = (distance head item))
                  (mini distance)
                  (when (= mini distance)
                    (setf result i))))
              result))
           ((:labels impl (node))
            (bind ((children (read-children node))
                   (full (eql (length children) %branching-factor)))
              (if full
                  (bind ((close-index (closest-index children))
                         (next-node (aref children close-index)))
                    (impl next-node)
                    (update-ranges! container node item close-index))
                  (bind ((new-bucket (apply #'cl-ds:make-bucket
                                            operation
                                            container
                                            item
                                            additional-arguments))
                         (last (length children))
                         (range-size `(,%branching-factor ,%branching-factor))
                         (close-range (make-array range-size
                                                  :element-type %metric-type))
                         (distant-range (make-array range-size
                                                    :element-type %metric-type))
                         (new-node (make-instance 'egnat-node
                                                  :content (vect new-bucket)
                                                  :close-range close-range
                                                  :distant-range distant-range)))
                    (push-children! node new-node)
                    (reinitialize-ranges! container node)
                    (update-ranges! container node item last))))))
      (impl (access-root container))
      (incf %size)))
  (values container
          cl-ds.common:empty-eager-modification-operation-status))


(-> egnat-replace! (mutable-egnat-container cl-ds:grow-function t egnat-node list)
    (values mutable-egnat-container
            cl-ds.common:eager-modification-operation-status))
(defun egnat-replace! (container operation
                       item last-node
                       additional-arguments)
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
    (values container status)))


(-> egnat-push! (mutable-egnat-container cl-ds:grow-function t egnat-node list)
    (values mutable-egnat-container
            cl-ds.common:eager-modification-operation-status))
(defun egnat-push! (container operation
                    item node
                    additional-arguments)
  (bind ((content (read-content node))
         (new-bucket (apply #'cl-ds:make-bucket
                            operation
                            container
                            item
                            additional-arguments)))
    (incf (access-size container))
    (vector-push-extend new-bucket content)
    (values container
            cl-ds.common:empty-eager-modification-operation-status)))


(-> egnat-grow! (mutable-egnat-container cl-ds:grow-function t list)
    (values mutable-egnat-container
            cl-ds.common:eager-modification-operation-status))
(defun egnat-grow! (container operation item additional-arguments)
  (if (~> container access-root null) ; border case. nil is valid value for root
      (initialize-root! container operation item additional-arguments)
      (bind (((:slots %metric-f %same-fn %content-count-in-node
                      %size %root %branching-factor)
              container)
             ((:values paths found last-node) (find-destination-node container
                                                                     item)))
        #|
following cases need to be considered:
1) item already present in the egnat.
   Simply attempt to change bucket, and roll with result
2) item not present in the egnat, but there is a node that we can
   stick it in, without updating ranges. Do it and be happy.
3) item not present in the egnat, and every node in path is already full.
   Find first node (from the root) that can hold another children. Create node
   by spliting content of parent in two. Update ranges of parent since new
   children has been added. Then update ranges of whole path because new item
   has been added.
        |#
        (if found ; case number 1, easy to handle
            (egnat-replace! container operation
                            item last-node
                            additional-arguments)
            (iterate
              (for (node parent) in-hashtable paths)
              (for content = (read-content node))
              (finding node
                       such-that (< (fill-pointer content)
                                    %content-count-in-node)
                       into result)
              (finally
               ;; checking if it is the case number 2
               (if (null result)
                   ;; case 3, it will be messy...
                   (return (splitting-grow! container operation item
                                            additional-arguments))
                   ;; the case number 2, just one push-extend and we are done
                   (return (egnat-push! container operation item result
                                        additional-arguments)))))))))


(defun walk-path (fn node possible-paths)
  (iterate
    (for p
         initially (cons node 0)
         then (gethash (car p) possible-paths))
    (until (null p))
    (funcall fn p)))


(-> merging-shrink! (mutable-egnat-container
                     egnat-node t hash-table fixnum)
    t)
(defun merging-shrink! (container node item paths position)
  "Removes element from node. Takes in account potential head change, updates ranges."
  cl-ds.utils:todo)


(-> remove-from-node! (mutable-egnat-container
                       cl-ds:shrink-function
                       egnat-node t
                       hash-table list)
    (values mutable-egnat-container
            cl-ds.common:eager-modification-operation-status))
(defun remove-from-node! (container operation node item paths
                          additional-arguments)
  (bind ((content (read-content node))
         (position (position item content :test (curry #'same container)))
         (bucket (aref content position))
         ((:values new-bucket status changed) (apply #'cl-ds:shrink-bucket!
                                                     operation
                                                     container
                                                     bucket
                                                     item
                                                     additional-arguments)))
    (when changed
        (if (null new-bucket)
            ;; remove from node, update paths, sometimes reinitialize paths...
            (merging-shrink! container node paths position)
            (setf (aref content position) new-bucket)))
    (values container status)))


(-> egnat-shrink! (mutable-egnat-container cl-ds:shrink-function t list)
    (values mutable-egnat-container
            cl-ds.common:eager-modification-operation-status))
(defun egnat-shrink! (container operation item additional-arguments)
  (if (~> container access-root null)
      (values container
              cl-ds.common:empty-eager-modification-operation-status)
      (bind (((:slots %metric-fn %same-fn %content-count-in-node %size
                      %root %branching-factor)
              container)
             ((:values paths found last-node) (find-destination-node container
                                                                     item)))
        (if found
            (remove-from-node! container operation last-node item paths
                               additional-arguments)
            (values container
                    cl-ds.common:empty-eager-modification-operation-status)))))
