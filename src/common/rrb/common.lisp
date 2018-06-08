;; link to java implementation https://github.com/clojure/clojure/blob/0b73494c3c855e54b1da591eeb687f24f608f346/src/jvm/clojure/lang/PersistentVector.java
(in-package #:cl-data-structures.common.rrb)


(define-constant +bit-count+ 5)
(define-constant +maximal-shift+ (iterate
                                   (for c
                                        initially most-positive-fixnum
                                        then (ash c (- +bit-count+)))
                                   (until (zerop c))
                                   (counting t)))
(define-constant +maximum-children-count+ (ash 1 +bit-count+))
(define-constant +tail-mask+ (dpb 0 (byte +bit-count+ 0) most-positive-fixnum))


(deftype node-content ()
  `(simple-vector ,+maximum-children-count+))
(deftype rrb-node ()
  'node-content)
(deftype node-size ()
  `(integer 0 ,+maximum-children-count+))
(deftype shift ()
  `(integer 0 ,+maximal-shift+))
(deftype rrb-index ()
  `(integer 0 ,(ash 1 (* +bit-count+ +maximal-shift+))))
(deftype rrb-indexes ()
  `(simple-array node-size (,+maximal-shift+)))
(deftype rrb-path ()
  `(simple-array * (,+maximal-shift+)))

(defun make-node-content (&optional (element-type t))
  (make-array +maximum-children-count+ :initial-element nil
                                       :element-type element-type))


(defun make-rrb-node (&key (content (make-node-content)) ownership-tag)
  (register-ownership ownership-tag content)
  content)


(defun rrb-node-deep-copy (node ownership-tag)
  (lret ((result (copy-array node)))
    (register-ownership result ownership-tag)
    result))


(defun rrb-node-push! (node position element)
  (setf (aref node position) element)
  node)


(defun rrb-node-push-into-copy (node position element ownership-tag)
  (let* ((source-content node)
         (result-content (make-node-content (array-element-type source-content))))
    (setf (aref result-content position) element)
    (iterate
      (for i from 0 below position)
      (setf (aref result-content i) (aref source-content i)))
    (make-rrb-node :ownership-tag ownership-tag
                   :content result-content)))


(defun rrb-node-pop-in-the-copy (node position ownership-tag)
  (unless (zerop position)
    (let* ((source-content node)
           (result-content (copy-array source-content)))
      (setf (aref result-content position) nil)
      (make-rrb-node :ownership-tag ownership-tag
                     :content result-content))))


(defun rrb-node-pop! (node position)
  (setf (aref node position) nil)
  node)


(defclass rrb-container (fundamental-ownership-tagged-object
                         cl-ds:traversable)
  ((%root :accessor access-root
          :initarg :root
          :initform nil
          :type (or rrb-node nil)
          :documentation "root of the tree")
   (%shift :initarg :shift
           :accessor access-shift
           :type shift
           :initform 0)
   (%size :initarg :size
          :initform 0
          :type non-negative-fixnum
          :accessor access-size)
   (%element-type :initarg :element-type
                  :initform t
                  :reader read-element-type)
   (%tail-size :initform 0
               :initarg :tail-size
               :type node-size
               :accessor access-tail-size)
   (%tail :initform nil
          :type (or null simple-array)
          :initarg :tail
          :accessor access-tail)))


(defmethod initialize-instance :after ((container rrb-container) &key &allow-other-keys)
  (bind (((:slots %tail %tail-size %size %shift %root) container)
         (max-size (ash +maximum-children-count+ (* +bit-count+ %shift))))
    (assert (<= 0 %shift +maximal-shift+))
    (assert (<= 0 %tail-size +maximum-children-count+))
    (assert (<= 0 %size))
    (assert (<= %size max-size))
    (assert (if (null %tail) (zerop %tail-size) t))))


(declaim (inline tail-offset))
(-> tail-offset (non-negative-fixnum) non-negative-fixnum)
(defun tail-offset (size)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (if (< size 32)
      0
      (~> size 1- (logand +tail-mask+))))


(defun push-into-copy-of-tail (rrb-container ownership-tag element)
  (bind (((:slots %tail-size %tail) rrb-container))
    (unless (eql %tail-size +maximum-children-count+)
      (bind ((content %tail)
             ((:vectors copy-of-content) (copy-array content)))
        (setf (copy-of-content %tail-size) element)
        (make-rrb-node :ownership-tag ownership-tag
                       :content copy-of-content)))))


(defun push-into-tail! (rrb-container element)
  (bind (((:slots %tail-size %tail) rrb-container))
    (unless (eql %tail-size +maximum-children-count+)
      (bind (((:vectors content) %tail))
        (setf (content %tail-size) element)
        t))))


(declaim (inline insert-tail))
(-> insert-tail (rrb-container
                 t
                 (-> (rrb-path rrb-indexes shift t node-content)
                     cl-ds.common.rrb:rrb-node)
                 node-content)
    rrb-node)
(defun insert-tail (rrb-container ownership-tag continue tail)
  (declare (optimize (speed 3) (safety 1)
                     (space 0) (debug 0)))
  (bind (((:slots %size %shift %root) rrb-container)
         (root-overflow (>= (the fixnum (ash (the fixnum %size) (- +bit-count+)))
                            (ash 1 (* +bit-count+ (the shift %shift))))))
    (if root-overflow
        (iterate
          (repeat %shift)
          (for node
               initially (make-rrb-node :content tail
                                        :ownership-tag ownership-tag)
               then (let ((next (make-rrb-node :ownership-tag ownership-tag)))
                      (setf (~> next (aref 0))
                            node)
                      next))
          (finally
           (bind ((root (make-rrb-node :ownership-tag ownership-tag))
                  ((:vectors content) root))
             (setf (content 0) %root
                   (content 1) node)
             (return (values root t)))))
        (let ((path (make-array +maximal-shift+
                                :initial-element nil))
              (indexes (make-array +maximal-shift+
                                   :element-type `(integer 0 ,+maximum-children-count+))))
          (declare (dynamic-extent path)
                   (dynamic-extent indexes))
          (iterate
            (with size = (the non-negative-fixnum %size))
            (for i from 0 below %shift)
            (for position from (* +bit-count+ %shift) downto 0 by +bit-count+)
            (for index = (ldb (byte +bit-count+ position) size))
            (for node
                 initially %root
                 then (and node (~> node (aref index))))
            (setf (aref path i) node
                  (aref indexes i) index))
          (values (funcall continue
                           path
                           indexes
                           %shift
                           ownership-tag
                           tail)
                  nil)))))


(-> descend-into-tree
    (rrb-container
     rrb-index
     (-> (rrb-path rrb-indexes shift) t))
    t)
(defun descend-into-tree (rrb-container location continue)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0)))
  (let ((path (make-array +maximal-shift+
                          :initial-element nil))
        (shift (access-shift rrb-container))
        (indexes (make-array +maximal-shift+
                             :element-type `(integer 0 ,+maximum-children-count+))))
    (declare (dynamic-extent path)
             (dynamic-extent indexes))
    (iterate
      (for i from 0 to shift)
      (for position from (* +bit-count+ shift) downto 0 by +bit-count+)
      (for index = (ldb (byte +bit-count+ position) location))
      (for node
           initially (access-root rrb-container)
           then (and node (~> node (aref index))))
      (setf (aref path i) node
            (aref indexes i) index))
    (funcall continue
             path
             indexes
             shift)))


(-> rrb-at (rrb-container rrb-index) t)
(defun rrb-at (container index)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0)))
  (if (< index (access-size container))
      (iterate
        (with shift = (slot-value container '%shift))
        (for position from (* +bit-count+ shift) downto 0 by +bit-count+)
        (for i = (ldb (byte +bit-count+ position) index))
        (for node
             initially (slot-value container '%root)
             then (~> node (aref i)))
        (finally (return node)))
      (let ((offset (- index (access-size container))))
        (~> container access-tail (aref offset)))))


(defmethod cl-ds:at ((container rrb-container) index &rest more)
  (cl-ds:assert-one-dimension more)
  (unless (< -1 index (cl-ds:size container))
    (error 'cl-ds:argument-out-of-bounds
           :argument 'index
           :bounds (list 0 (cl-ds:size container))
           :value index
           :text "Index out of bounds."))
  (check-type index rrb-index)
  (rrb-at container index))


(declaim (inline copy-on-write))
(-> copy-on-write
    (rrb-path rrb-indexes shift t node-content)
    cl-ds.common.rrb:rrb-node)
(defun copy-on-write (path indexes shift ownership-tag tail)
  (declare (optimize (speed 3) (debug 0)
                     (space 0) (safety 1)))
  (iterate
    (for i from (1- shift) downto 0)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         initially (make-rrb-node :content tail
                                  :ownership-tag ownership-tag)
         then (if (null old-node)
                  (lret ((n (make-rrb-node :content (make-node-content (array-element-type tail))
                                           :ownership-tag ownership-tag)))
                    (setf (~> n (aref position)) node))
                  (rrb-node-push-into-copy old-node
                                           position
                                           node
                                           ownership-tag)))
    (finally (return node))))


(defun acquire-path (path shift ownership-tag)
  (or
   (iterate
     (for i from 0 below shift)
     (for node = (aref path i))
     (finding i such-that (~> node
                              (acquire-ownership ownership-tag)
                              null)))
   shift))


(-> transactional-copy-on-write
    (rrb-path rrb-indexes shift t cl-ds.common.rrb:node-content)
    cl-ds.common.rrb:rrb-node)
(defun transactional-copy-on-write (path indexes shift ownership-tag tail)
  (iterate
    (with acquired = (acquire-path path shift ownership-tag))
    (for i from (1- shift) downto 0)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         initially (make-rrb-node :content tail
                                  :ownership-tag ownership-tag)
         then (if (null old-node)
                  (lret ((n (make-rrb-node :content (make-node-content (array-element-type tail))
                                           :ownership-tag ownership-tag)))
                    (setf (~> n (aref position)) node))
                  (if (< i acquired)
                      (rrb-node-push! old-node
                                      position
                                      node)
                      (rrb-node-push-into-copy old-node
                                               position
                                               node
                                               ownership-tag))))
    (finally (return node))))


(declaim (inline destructive-write))
(-> destructive-write
    (rrb-path rrb-indexes shift t cl-ds.common.rrb:node-content)
    cl-ds.common.rrb:rrb-node)
(defun destructive-write (path indexes shift ownership-tag tail)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0)))
  (iterate
    (for i from 0 below shift)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         initially (make-rrb-node :content tail
                                  :ownership-tag ownership-tag)
         then (if (null old-node)
                  (lret ((n (make-rrb-node :content (make-node-content (array-element-type tail))
                                           :ownership-tag ownership-tag)))
                    (setf (~> n (aref position)) node))
                  (rrb-node-push! old-node position node)))
    (finally (return node))))


(-> remove-tail (rrb-container) t)
(defun remove-tail (rrb-container)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0)))
  (bind (((:slots %size %shift %root) rrb-container)
         (root-underflow (eql (ash (- %size +maximum-children-count+)
                                   (- (* %shift +bit-count+)))
                              1)))
    (if (zerop %shift)
        (values nil %root nil)
        (iterate
          (with last = (1- (the non-negative-fixnum %size)))
          (repeat %shift)
          (for position
               from (* +bit-count+ %shift)
               downto 0
               by +bit-count+)
          (for index = (ldb (byte +bit-count+ position) last))
          (for node
               initially %root
               then (~> node (aref index)))
          (finally
           (check-type node rrb-node)
           (return
             (values
              (if root-underflow
                  (~> %root (aref 0))
                  %root)
              node
              root-underflow)))))))


(defmethod cl-ds:size ((container rrb-container))
  (+ (access-size container)
     (access-tail-size container)))


(labels ((impl (function node depth)
           (if (zerop depth)
               (map nil function node)
               (map nil
                    (lambda (x) (impl function x (1- depth)))
                    node))))
  (defmethod cl-ds:traverse (function (object rrb-container))
    (let ((root (access-root object)))
      (unless (null root)
        (impl function root (access-shift object))))
    (iterate
      (with tail = (access-tail object))
      (for i from 0 below (access-tail-size object))
      (funcall function (aref tail i)))
    object))


(defclass rrb-range (cl-ds:fundamental-random-access-range)
  ((%start :type fixnum
           :accessor access-start)
   (%last-size :type fixnum
               :accessor access-last-size)
   (%lower-bound :type fixnum
                 :accessor access-lower-bound)
   (%upper-bound :initarg :upper-bound
                 :type fixnum
                 :accessor access-upper-bound)
   (%initial-lower-bound :initarg :initial-lower-bound
                         :type fixnum
                         :reader read-initial-lower-bound)
   (%initial-upper-bound :initarg :initial-upper-bound
                         :type fixnum
                         :reader read-initial-upper-bound)
   (%content :reader read-content)
   (%container :initarg :container
               :accessor access-container)))


(defclass mutable-rrb-range (rrb-range)
  ())


(defmethod cl-ds:whole-range ((container rrb-container))
  (make 'rrb-range :container container
                   :initial-lower-bound 0
                   :initial-upper-bound (cl-ds:size container)))


(defmethod cl-ds:whole-range ((container mutable-rrb-range))
  (make 'mutable-rrb-range :container container
                           :initial-lower-bound 0
                           :initial-upper-bound (cl-ds:size container)))


(defun init-rrb (instance container &key (from 0) (to (cl-ds:size container)))
  (bind (((:slots %start %last-size %content %lower-bound
                  %upper-bound %size)
          instance)
         ((:slots %root %shift %size %tail-size %tail)
          container))
    (setf %content (make-instance 'flexichain:standard-flexichain
                                  :min-size +maximum-children-count+)
          %start (mod from +maximum-children-count+)
          %lower-bound from
          %last-size (mod to +maximum-children-count+)
          %upper-bound to)
    (labels ((collect-bottom (node depth &optional (offset 0))
               (declare (optimize (debug 3)))
               (let* ((bit-offset (* +bit-count+ (- %shift depth)))
                      (start-range offset)
                      (difference (ash 1 bit-offset))
                      (end-range (+ offset (ash +tail-mask+ bit-offset))))
                 (when (<= start-range from to end-range)
                   (if (zerop depth)
                       (flexichain:push-end %content node)
                       (iterate
                         (for i from 0 below +maximum-children-count+)
                         (for d from difference by difference)
                         (collect-bottom (~> node (aref i))
                                         (1- depth)
                                         (+ start-range d))))))))
      (collect-bottom %root %shift))
    (unless (null %tail)
      (when (< %size to)
        (flexichain:push-end %content %tail)))))


(defmethod initialize-instance :after ((instance rrb-range)
                                       &key container
                                       &allow-other-keys)
  (init-rrb instance container
            :from (read-initial-lower-bound instance)
            :to (read-initial-upper-bound instance)))


(defmethod reinitialize-instance (instance &key &allow-other-keys)
  (init-rrb instance (access-container instance)
            :from (read-initial-lower-bound instance)
            :to (read-initial-upper-bound instance)))


(defmethod cl-ds:peek-front ((range rrb-range))
  (bind (((:slots %start %content) range))
    (if (null %content)
        (values nil nil)
        (~> (flexichain:element* %content 0)
            (aref %start)
            (values t)))))


(defmethod cl-ds:peek-back ((range rrb-range))
  (bind (((:slots %tail-size %content) range))
    (if (null %content)
        (values nil nil)
        (~> %content
            (flexichain:element* (~> %content flexichain:nb-elements 1-))
            (aref (1- %tail-size))
            (values t)))))


(defmethod cl-ds:at ((range rrb-range) index &rest more)
  (cl-ds:assert-one-dimension more)
  (bind (((:slots %upper-bound %lower-bound %content) range))
    (unless (and (>= index %lower-bound) (< index %upper-bound))
      (error 'cl-ds:argument-out-of-bounds
             :argument 'index
             :bounds (list %lower-bound %upper-bound)
             :value index
             :text "Index out of bounds."))
    (let* ((index (- index %lower-bound))
           (which-array (ash index (- +bit-count+)))
           (array-index (logand index (lognot +tail-mask+))))
      (~> %content
          (flexichain:element* which-array)
          (aref array-index)))))


(defmethod cl-ds:consume-front ((range rrb-range))
  (bind (((:slots %start %content %last-size %lower-bound %upper-bound)
          range))
    (if (or (eql %lower-bound %upper-bound) (null %content))
        (values nil nil)
        (let* ((new-start (rem (1+ %start) +maximum-children-count+))
               (old-start %start)
               (reached-tail (eql 1 (flexichain:nb-elements %content)))
               (first-array (flexichain:element* %content 0)))
          (when (and reached-tail (eql old-start %last-size))
            (setf %content nil)
            (return-from cl-ds:consume-front (values nil nil)))
          (when (zerop new-start)
            (flexichain:pop-start %content))
          (setf %start new-start)
          (incf %lower-bound)
          (~> first-array
              (aref old-start)
              (values t))))))


(defmethod cl-ds:consume-back ((range rrb-range))
  (bind (((:slots %start %content %last-size %upper-bound) range))
    (if (null %content)
        (values nil nil)
        (let* ((new-end (if (eql %last-size 1)
                            +maximum-children-count+
                            (1- %last-size)))
               (old-end %last-size)
               (position (1- %last-size))
               (reached-tail (eql 1 (flexichain:nb-elements %content)))
               (last-array (~>> %content
                                flexichain:nb-elements
                                1-
                                (flexichain:element* %content))))
          (when (and reached-tail (eql %start position))
            (setf %content nil)
            (return-from cl-ds:consume-back (values nil nil)))
          (when (eql old-end 1)
            (flexichain:pop-end %content))
          (setf %last-size new-end)
          (decf %upper-bound)
          (~> last-array
              (aref position)
              (values t))))))


(defmethod cl-ds:across (function (range rrb-range))
  (bind (((:slots %start %lower-bound %upper-bound %content %last-size) range)
         (index %start)
         (last-position (~> %content flexichain:nb-elements 1-)))
    (iterate
      (for i from 0 below (flexichain:nb-elements %content))
      (for array = (flexichain:element* %content i))
      (for end = (eql i last-position))
      (iterate
        (for a from index below (if end %last-size +maximum-children-count+))
        (funcall function (aref array a)))
      (setf index 0))
    range))


(defmethod cl-ds:traverse (function (range rrb-range))
  (bind (((:slots %start %lower-bound %upper-bound %content %last-size) range)
         (index %start)
         (last-position (~> %content flexichain:nb-elements 1-)))
    (iterate
      (for i from 0)
      (until (eql %lower-bound %upper-bound))
      (until (zerop (flexichain:nb-elements %content)))
      (for array = (flexichain:element* %content 0))
      (iterate
        (with end = (eql i last-position))
        (with last-index = (if end %last-size +maximum-children-count+))
        (for a from index below last-index)
        (setf %start (1+ a))
        (incf %lower-bound)
        (when (eql %start last-index)
          (flexichain:pop-start %content))
        (funcall function (aref array a))
        (until (eql %lower-bound %upper-bound)))
      (setf index 0))
    range))


(defmethod cl-ds:reset! ((obj rrb-range))
  (reinitialize-instance obj)
  obj)


(defmethod cl-ds:size ((obj rrb-range))
  (bind (((:slots %upper-bound %lower-bound) obj))
    (- %upper-bound %lower-bound)))


(defmethod cl-ds:clone ((obj rrb-range))
  (bind (((:slots %lower-bound %upper-bound %container) obj))
    (make (type-of obj)
          :container %container
          :initial-lower-bound %lower-bound
          :initial-upper-bound %upper-bound)))


(defmethod (setf cl-ds:peek-back) (new-value (range mutable-rrb-range))
  (bind (((:slots %tail-size %content) range))
    (if (null %content)
        (error 'cl-ds:operation-not-allowed
               :text "Can't assign into empty range!")
        (setf (aref (~> %content
                        (flexichain:element* (~> %content
                                                 flexichain:nb-elements
                                                 1-)))
                    (1- %tail-size))
              new-value))))


(defmethod (setf cl-ds:peek-front) (new-value (range mutable-rrb-range))
  (bind (((:slots %start %content) range))
    (if (null %content)
        (error 'cl-ds:operation-not-allowed
               :text "Can't assign into empty range!")
        (setf (aref (flexichain:element* %content 0)
                    %start)
              new-value))))


(defmethod (setf cl-ds:at) (new-value (range mutable-rrb-range) index &rest more)
  (cl-ds:assert-one-dimension more)
  (bind (((:slots %upper-bound %lower-bound %content) range))
    (unless (and (>= index %lower-bound) (< index %upper-bound))
      (error 'cl-ds:argument-out-of-bounds
             :argument 'index
             :bounds (list %lower-bound %upper-bound)
             :value index
             :text "Index out of bounds."))
    (let* ((index (- index %lower-bound))
           (which-array (ash index (- +bit-count+)))
           (array-index (logand index (lognot +tail-mask+))))
      (setf (aref (~> %content
                      (flexichain:element* which-array))
                  array-index)
            new-value))))
