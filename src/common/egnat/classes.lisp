(in-package #:cl-data-structures.common.egnat)


(defclass fundamental-egnat-container ()
  ((%branching-factor
    :type non-negative-fixnum
    :initarg :branching-factor
    :reader read-branching-factor)
   (%metric-fn
    :reader read-metric-fn
    :initarg :metric-fn)
   (%same-fn
    :reader read-same-fn
    :initarg :same-fn)
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
    :accessor access-root
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


(defclass egnat-range (cl-ds:fundamental-forward-range)
  ((%stack :initform nil
           :initarg :stack
           :accessor access-stack)
   (%initial-stack :reader read-initial-stack
                   :initarg :initial-stack)
   (%container :initarg :container
               :reader read-container)))


(defclass egnat-range-around (egnat-range)
  ((%near :initarg :near
          :reader read-near)
   (%margin :initarg :margin
            :reader read-margin)))


(defclass egnat-grow-range (egnat-around)
  ((%near :initarg :near
          :reader read-near)
   (%possible-paths :initform (vect)
                    :reader read-possible-paths)))


(defmethod initialize-instance :after ((obj egnat-grow-range)
                                       &key &allow-other-keys)
  (bind (((:slots %possible-paths %stack) obj))
    (vector-push-extend %stack %possible-paths)))


(defmethod (setf access-stack)
    :before (new-value (obj egnat-grow-range))
  (bind (((:slots %possible-paths) obj)
         (length (length %possible-paths))
         ((node . _) (aref %possible-paths (1- length)))
         ((new-node . _) (first new-value)))
    (unless (eq node new-node)
      (vector-push-extend new-value %possible-paths))))


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
             :class (type-of container)
             :argument :content-count-in-node
             :bounds '(>= 1)))
    (when (< %branching-factor 2)
      (error 'cl-ds:initialization-out-of-bounds
             :text "Branching factor should be at least 2"
             :value %branching-factor
             :class (type-of container)
             :argument :branching-factor
             :bounds '(>= 2)))))
