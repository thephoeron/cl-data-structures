(in-package #:cl-data-structures.common.egnat)


(defclass fundamental-egnat-container ()
  ((%branching-factor
    :type non-negative-fixnum
    :initarg :branching-factor
    :reader read-branching-factor)
   (%metric-fn
    :reader read-metric-fn
    :initarg :metric-fn)
   (%metric-type
    :reader read-metric-type
    :initform :single-float
    :initarg :metric-type)
   (%content-count-in-node
    :type non-negative-fixnum
    :reader read-content-count-in-node
    :initarg :content-count-in-node)
   (%size
    :reader read-size
    :type non-negative-fixnum
    :initform 0)
   (%root
    :reader read-root
    :initform nil
    :initarg :root)))


(defclass egnat-node ()
  ((%close-range
    :initform nil
    :initarg :close-range
    :reader read-close-range)
   (%distant-range
    :initarg :distant-range
    :initform nil
    :reader read-distant-range)
   (%content
    :type vector
    :initarg :content
    :reader read-content)
   (%children
    :type (or null gnat-node)
    :initform nil
    :initarg :children
    :reader read-children)))


(defmethod initialize-instance
    :after ((container fundamental-egnat-container)
            &rest all)
  (declare (ignore all))
  (bind (((:slots %branching-factor %content-count-in-node) container))
    (check-type %branching-factor integer)
    (check-type %content-count-in-node integer)
    (when (zerop %content-count-in-node)
      (error 'cl-ds:initialization-out-of-bounds
             :text "Content count in node should be at least 1"
             :value %content-count-in-node
             :argument :content-count-in-node
             :bounds '(>= 1)))
    (when (< %branching-factor 2)
      (error 'cl-ds:initialization-out-of-bounds
             :text "Branching factor should be at least 2"
             :value %branching-factor
             :argument :branching-factor
             :bounts '(>= 2)))))
