(in-package #:cl-data-structures.common.egnat)


(defun force-tree (node)
  (bind (((:slots %children) node))
    (unless (null %children)
      (map-into %children #'lparallel:force %children)
      (map nil #'force-tree %children)))
  node)


(defun splice-content (data count)
  (let* ((size (cl-ds:size data))
         (content (cl-ds.alg:sequence-window data 0 count))
         (new-data (cl-ds.alg:sequence-window data count (cl-ds:size data))))
    (assert (> size count))
    (assert (eql count (cl-ds:size content)))
    (values
     content
     new-data)))


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


(defun make-partitions (seeds-indexes data metric-fn)
  (let ((result (make-array (cl-ds:size data) :element-type 'fixnum))
        (index 0))
    (cl-ds:across
     (lambda (value)
       (setf (aref result (finc index))
             (iterate
               (for seed-index in-vector seeds-indexes)
               (for seed = (cl-ds:at data seed-index))
               (for distance = (funcall metric-fn value seed))
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
    (map-into result #'vect)
    (iterate
      (for d in-vector data-partitions)
      (for i from 0)
      (for element = (cl-ds:at data i))
      (for partition = (gethash d reverse-mapping))
      ;; don't assign seeds to partitions because it has been already done
      ;; in map-into form
      (unless (eql d i)
        (vector-push-extend element (aref result partition))))
    result))


(defun make-ranges (heads contents branching-factor metric-type metric-fn)
  (bind ((close-range (make-array `(,branching-factor ,branching-factor)
                                  :element-type metric-type))
         (distant-range (make-array `(,branching-factor ,branching-factor)
                                    :element-type metric-type)))
    (iterate
      (for child from 0 below (length contents))
      (for head in-vector heads)
      (iterate
        (for data from 0 below (length contents))
        (for init = (funcall metric-fn head (aref heads data)))
        (unless (eql child data)
          (multiple-value-bind (min max)
              (cl-ds.utils:optimize-value ((mini < init)
                                           (maxi < init))
                (map nil (lambda (x)
                           (let ((distance (coerce (funcall metric-fn x head)
                                                   metric-type)))
                             (mini distance)
                             (maxi distance)))
                     (aref contents data)))
            (setf (aref close-range child data) min
                  (aref distant-range child data) max)))))
    (values close-range distant-range)))


(defun make-future-egnat-subtrees (operation container
                                   extra-arguments head data)
  (bind (((:slots %metric-fn %metric-type
                  %content-count-in-node %branching-factor)
          container)
         ((:values this-content this-data)
          (splice-content data %content-count-in-node))
         ((:values seeds-indexes reverse-mapping)
          (select-seeds %branching-factor (cl-ds:size this-data)))
         (data-partitions (make-partitions seeds-indexes
                                           this-data
                                           %metric-fn))
         (contents (make-contents seeds-indexes this-data
                                  data-partitions reverse-mapping))
         (heads (map 'vector (curry #'cl-ds:at this-data)
                     seeds-indexes))
         (children (map 'vector
                        (lambda (head content)
                          (lparallel:future
                            (make-future-egnat-nodes operation
                                                     container
                                                     extra-arguments
                                                     head
                                                     content)))
                        heads
                        contents))
         ((:values close-range distant-range) (make-ranges heads
                                                           contents
                                                           %branching-factor
                                                           %metric-type
                                                           %metric-fn)))
    (assert (= (reduce #'+ contents
                       :key #'length
                       :initial-value (length contents))
               (cl-ds:size this-data)))
    (make 'egnat-node
          :content (apply #'cl-ds:make-bucket
                          operation
                          container
                          this-content
                          :head head
                          extra-arguments)
          :close-range close-range
          :distant-range distant-range
          :children children)))


(defun make-future-egnat-nodes (operation container extra-arguments head data)
  (if (<= (1- (cl-ds:size data)) (read-content-count-in-node container))
      (make 'egnat-node :content (apply #'cl-ds:make-bucket
                                        operation
                                        container
                                        data
                                        :head head
                                        extra-arguments))
      (make-future-egnat-subtrees operation container
                                  extra-arguments head data)))


(defun make-egnat-tree (operation container extra-arguments data)
  (let ((head (cl-ds:at data 0))
        (data (cl-ds.alg:sequence-window data 1 (cl-ds:size data))))
    (~>> (make-future-egnat-nodes operation container extra-arguments
                                  head data)
         force-tree)))


(defun prune-subtrees (trees close-range distant-range value metric-fn)
  (let ((result (make-array (length trees)
                            :element-type 'bit
                            :initial-element 1))
        (length (length trees)))
    (iterate
      (for i from 0)
      (for tree in-vector trees)
      (for content = (~> tree read-content (aref 0)))
      (for distance = (funcall metric-fn value content))
      (iterate
        (for j from 0 below length)
        (unless (or (eql i j) (zerop (aref result j)))
          (let ((in-range (<= (aref close-range i j)
                              distance
                              (aref distant-range i j))))
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
    (with metric-fn = (read-metric-fn container))
    (for node in-vector nodes)
    (for content = (read-content node))
    (for head = (bucket-head container content))
    (for distance = (funcall metric-fn head item))
    (minimize distance into min)
    (for result
         initially node
         then (if (= distance min)
                  node
                  result))
    (finally (return result))))
