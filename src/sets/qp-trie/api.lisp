(cl:in-package #:cl-data-structures.sets.qp-trie)


(define-condition empty-array-key (cl-ds:invalid-value)
  ()
  (:default-initargs :format-control "Empty array as key in the qp-trie-set.~%"))


(defclass fundamental-qp-trie-set (cl-data-structures.common.qp-trie:qp-trie)
  ((%size :type non-negative-integer
           :initarg :size
           :reader cl-ds:size
           :accessor access-size))
  (:default-initargs
   :size 0))


(defclass mutable-qp-trie-set (fundamental-qp-trie-set
                               cl-ds.sets:mutable-set)
  ())



(defmethod cl-ds.meta:position-modification
    ((function cl-ds.meta:put!-function)
     (structure mutable-qp-trie-set)
     container
     location
     &key value)
  (declare (ignore value))
  (check-type location (simple-array (unsigned-byte 8)))
  (when (emptyp location)
    (error 'empty-array-key :value location))
  (let ((result (cl-ds.common.qp-trie:qp-trie-insert! structure location)))
    (when result (incf (access-size structure)))
    (values structure
            (cl-ds.common:make-eager-modification-operation-status
             (not result)
             (if result nil location)
             result))))


(defmethod cl-ds.meta:position-modification
    ((function cl-ds.meta:erase!-function)
     (structure mutable-qp-trie-set)
     container
     location
     &rest all)
  (check-type location (simple-array (unsigned-byte 8)))
  (when (emptyp location)
    (error 'empty-array-key :value location))
  (let ((result (cl-ds.common.qp-trie:qp-trie-delete! structure location)))
    (when result (decf (access-size structure)))
    (values structure
            (cl-ds.common:make-eager-modification-operation-status
             result
             (if result location nil)
             result))))


(defmethod cl-ds:at ((container fundamental-qp-trie-set)
                     location
                     &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (check-type location (simple-array (unsigned-byte 8)))
  (when (emptyp location)
    (error 'empty-array-key :value location))
  (let* ((length (length location))
         (result (cl-ds.common.qp-trie:qp-trie-find container location))
         (result-found (eql length result)))
    (values result-found result-found)))


(defmethod cl-ds:empty-clone ((container fundamental-qp-trie-set))
  (~> container class-of make))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'mutable-qp-trie-set))
                                        &rest arguments)
  (declare (ignore arguments))
  (lret ((result (make 'mutable-qp-trie-set)))
    (cl-ds:across (lambda (key) (cl-ds:put! result key))
                  traversable)))


(defmethod cl-ds:across ((object fundamental-qp-trie-set)
                         function)
  (ensure-functionf function)
  (~>> object cl-ds.common.qp-trie:access-root
       (cl-ds.common.qp-trie:map-qp-trie-node function))
  object)


(defmethod cl-ds:clone ((object fundamental-qp-trie-set))
  (make (class-of object)
        :size (cl-ds:size object)
        :root (~> object cl-ds.common.qp-trie:access-root
                  cl-ds.common.qp-trie:qp-trie-node-clone)))


(defmethod cl-ds:reset! ((object mutable-qp-trie-set))
  (setf (access-size object) 0
        (cl-ds.common.qp-trie:access-root object)
        (cl-ds.common.qp-trie:make-qp-trie-node))
  object)


(defmethod cl-ds:whole-range ((object fundamental-qp-trie-set))
  cl-ds.utils:todo)
