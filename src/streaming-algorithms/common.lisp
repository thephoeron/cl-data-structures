(cl:in-package #:cl-data-structures.streaming-algorithms)


(define-constant +long-prime+ 2937759779) ; 2937759779 7822488144566681323 836515013123303
(define-constant +max-64-bit+ #XFFFFFFFFFFFFFFFF)


(defclass fundamental-data-sketch ()
  ((%hash-fn :initarg :hash-fn
             :accessor access-hash-fn)))


(defmethod cl-ds.utils:cloning-information append
    ((sketch fundamental-data-sketch))
  '((:hash-fn access-hash-fn)))


(defmethod initialize-instance :after ((sketch fundamental-data-sketch)
                                       &rest all)
  (declare (ignore all))
  (ensure-functionf (access-hash-fn sketch)))


(defgeneric compatible-p (first-sketch &rest more-sketches)
  (:method :around ((a fundamental-data-sketch) &rest more-sketches)
    (unless (every (curry #'eq (class-of a))
                   (mapcar #'class-of more-sketches))
      (return-from compatible-p nil))
    (unless (every (curry #'eq (access-hash-fn a))
                   (mapcar #'access-hash-fn more-sketches))
      (warn "Hashing function objects in the sketches mismatches. This may be a problem…"))
    (call-next-method)))


(defgeneric clean-sketch (function &rest arguments &key))


(defgeneric union (first-sketch &rest more-sketches)
  (:method :around ((sketch fundamental-data-sketch) &rest more-sketches)
    (unless (apply #'compatible-p sketch more-sketches)
      (error 'cl-ds:incompatible-arguments
             :parameters '(sketch more-sketches)
             :values `(,sketch ,more-sketches)
             :format-control "Sketches passed to the union are not compatible."))
    (call-next-method)))
