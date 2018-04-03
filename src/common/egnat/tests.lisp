(in-package :cl-user)
(defpackage egnat-tests (:use :prove :cl :iterate))
(in-package :egnat-tests)


(defmethod cl-ds:make-bucket (operation container content
                              &rest all)
  (declare (ignore all))
  content)


(defmethod cl-ds:grow-bucket!
    ((operation cl-ds:insert-function)
     (container cl-ds.common.egnat:mutable-egnat-container)
     bucket
     location
     &rest all)
  (declare (ignore all))
  (values location
          (cl-ds.common:make-eager-modification-operation-status
           t
           bucket)
          t))

(defmethod cl-ds:shrink-bucket!
    ((operation cl-ds:erase!-function)
     (container cl-ds.common.egnat:mutable-egnat-container)
     bucket
     location
     &rest all)
  (declare (ignore all))
  (values
   'cl-ds:null-bucket
   (cl-ds.common:make-eager-modification-operation-status t bucket)
   t))

(plan 1460)

(let ((container (make-instance
                  'cl-ds.common.egnat:mutable-egnat-container
                  :branching-factor 5
                  :metric-fn #'logxor
                  :same-fn #'=
                  :metric-type 'fixnum
                  :content-count-in-node 5))
      (data (coerce (iterate
                      (with generator = (cl-ds.utils:lazy-shuffle 0 5000))
                      (repeat 50)
                      (collect (funcall generator)))
                    'vector)))
  (let ((root (cl-ds.common.egnat::make-egnat-tree container nil nil data))
        (content (serapeum:vect)))
    (setf (cl-ds.common.egnat::access-root container) root)
    (is (length (cl-ds.common.egnat::read-content root)) 5)
    (labels ((impl (root)
               (unless (null root)
                 (iterate
                   (for c in-vector (cl-ds.common.egnat::read-content root))
                   (vector-push-extend c content)))
               (if (null root)
                   0
                   (+ (length (cl-ds.common.egnat::read-content root))
                      (reduce #'+
                              (cl-ds.common.egnat::read-children root)
                              :key #'impl)))))
      (is (impl root) 50)
      (is (sort data #'<) (sort content #'<) :test #'serapeum:vector=))
    (let* ((children (cl-ds.common.egnat::read-children root))
           (close-range (cl-ds.common.egnat::read-close-range root))
           (distant-range (cl-ds.common.egnat::read-distant-range root))
           (selection (aref data 10))
           (subtrees (cl-ds.common.egnat::prune-subtrees container
                                                         children
                                                         close-range
                                                         distant-range
                                                         selection
                                                         5)))
      (labels ((impl (root)
                 (when root
                   (or
                    (find selection
                          (cl-ds.common.egnat::read-content root))
                    (some #'impl
                          (cl-ds.common.egnat::read-children root))))))
        (ok (iterate
              (for i in-vector subtrees)
              (for tree in-vector children)
              (for result = (impl tree))
              (count result)))))
    (let ((range (make-instance 'cl-ds.common.egnat::egnat-range
                                :container container
                                :stack (list (cons root 0)))))
      (iterate
        (for (values value more) = (cl-ds:consume-front range))
        (while more)
        (collect value into result)
        (finally (is (serapeum:~> result (sort #'<) (coerce 'vector))
                     data
                     :test #'serapeum:vector=))))
    (let ((range (make-instance 'cl-ds.common.egnat::egnat-range-around
                                :container container
                                :margin 5
                                :near (aref data 10)
                                :stack (list (cons root 0)))))
      (iterate
        (for (values value more) = (cl-ds:consume-front range))
        (while more)
        (collect value into result)
        (finally (ok (every (lambda (x) (<= (logxor x (aref data 10)) 5))
                            result)))))
    (setf (cl-ds.common.egnat::access-root container) root)
    (multiple-value-bind (possible-paths found last-node)
        (cl-ds.common.egnat::find-destination-node container
                                                   (aref data 10))
      (isnt (hash-table-count possible-paths) 0)
      (ok found)
      (ok (find (aref data 10)
                (cl-ds.common.egnat::read-content last-node))))
    (multiple-value-bind (container status)
        (cl-ds.common.egnat::egnat-grow! container
                                         #'(setf cl-ds:at)
                                         (aref data 5)
                                         nil)
      (declare (ignore container))
      (is (cl-ds:value status) (aref data 5))
      (ok (cl-ds:found status)))
    (let ((container (make-instance 'cl-ds.common.egnat:mutable-egnat-container
                                    :branching-factor 5
                                    :content-count-in-node 10)))
      (multiple-value-bind (container status)
          (cl-ds.common.egnat::egnat-grow! container #'(setf cl-ds:at) 1 nil)
        (is (cl-ds:found status) nil)
        (is (cl-ds:value status) nil)
        (is (cl-ds:size container) 1)
        (is (serapeum:~> container
                         cl-ds.common.egnat::access-root
                         cl-ds.common.egnat::read-content
                         (aref 0))
            1)))
    (multiple-value-bind (container status)
        (cl-ds.common.egnat::egnat-grow! container #'(setf cl-ds:at)
                                         5005 nil)
      (is (cl-ds:found status) nil)
      (is (cl-ds:value status) nil)
      (let ((generator (cl-ds:near container 5005 0)))
        (is (cl-ds:consume-front generator) 5005)
        (is (nth-value 1 (cl-ds:peek-front generator)) nil)))
    (let ((size (cl-ds:size container)))
      (multiple-value-bind (container status)
          (cl-ds.common.egnat::egnat-shrink! container #'cl-ds:erase! 5008 nil)
        (is (cl-ds:found status) nil)
        (is (cl-ds:value status) nil)
        (is (cl-ds:size container) size)))
    (let* ((close-range (make-array '(4 4) :initial-element 0))
           (distant-range (make-array '(4 4) :initial-element 0))
           (fake-node (make-instance 'cl-ds.common.egnat::egnat-node
                                     :children (serapeum:vect 0 1 2 3)
                                     :close-range close-range
                                     :distant-range distant-range)))
      (iterate
        (for i from 0 below 4)
        (setf (aref close-range i 1) 1
              (aref distant-range i 1) 1
              (aref close-range 1 i) 2
              (aref distant-range 1 i) 2)
        (setf (aref close-range i 3) 3
              (aref distant-range i 3) 3
              (aref close-range 3 i) 4
              (aref distant-range 3 i) 4))
      (cl-ds.common.egnat::remove-children! fake-node 1)
      (is (serapeum:~> fake-node cl-ds.common.egnat::read-children length) 3)
      (iterate
        (for i from 0 below 3)
        (when (eql i 1) (next-iteration))
        (is (aref close-range 1 i) 4)
        (is (aref distant-range 1 i) 4)
        (is (aref close-range i 1) 3)
        (is (aref distant-range i 1) 3)))
    (let* ((original-content (sort (cl-ds.common.egnat::collect-buckets root)
                                   #'<))
           (restructured
             (cl-ds.common.egnat::reorginize-tree container root))
           (restructured-content (sort (cl-ds.common.egnat::collect-buckets restructured)
                                       #'<)))
      (is restructured-content original-content :test #'equalp))
    (cl-ds.common.egnat::egnat-shrink! container #'cl-ds:erase! 5005 nil)
    (is (sort (serapeum:~> container
                           cl-ds.common.egnat::access-root
                           cl-ds.common.egnat::collect-buckets)
              #'<)
        (sort data #'<)
        :test #'equalp)
    (iterate
      (for elt in-vector data)
      (for generator = (cl-ds:near container elt 0))
      (for value = (cl-ds:consume-front generator))
      (for (values nothing more) = (cl-ds:consume-front generator))
      (is more nil)
      (is elt value))))

(is-error (make-instance 'cl-ds.common.egnat:mutable-egnat-container
                         :branching-factor 0
                         :content-count-in-node 1)
          'cl-ds:initialization-out-of-bounds)
(is-error (make-instance 'cl-ds.common.egnat:mutable-egnat-container
                         :branching-factor 5
                         :content-count-in-node 0)
          'cl-ds:initialization-out-of-bounds)

(let ((container (make-instance
                  'cl-ds.common.egnat:mutable-egnat-container
                  :branching-factor 5
                  :metric-fn #'logxor
                  :same-fn #'=
                  :metric-type 'fixnum
                  :content-count-in-node 5))
      (data (coerce (iterate
                      (with generator = (cl-ds.utils:lazy-shuffle 0 5000))
                      (repeat 50)
                      (collect (funcall generator)))
                    'vector)))
  (setf (cl-ds.common.egnat::access-root container) nil
        (cl-ds.common.egnat::access-size container) 0)
  (iterate
    (for elt in-vector data)
    (for i from 1)
    (cl-ds.common.egnat::egnat-grow! container #'(setf cl-ds:at) elt nil)
    (is (cl-ds:size container) i)
    (iterate
      (for j from 0 below i)
      (for d = (elt data j))
      (for result = (cl-ds:near container d 0))
      (for (values content more) = (cl-ds:consume-front result))
      (is content d))))

(finalize)
