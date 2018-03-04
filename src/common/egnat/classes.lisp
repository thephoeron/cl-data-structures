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
