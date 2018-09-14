(in-package #:cl-data-structures)


(defgeneric at (container location &rest more-locations))

(defgeneric (setf at) (new-value container location &rest more-locations)
  (:generic-function-class cl-ds.meta:insert!-function))

(defgeneric add (container location new-value)
  (:generic-function-class cl-ds.meta:functional-add-function))

(defgeneric put (container item)
  (:generic-function-class cl-ds.meta:functional-put-function))

(defgeneric put-back (container item)
  (:generic-function-class cl-ds.meta:functional-put-back-function))

(defgeneric put-front (container item)
  (:generic-function-class cl-ds.meta:functional-put-front-function))

(defgeneric put-back! (container item)
  (:generic-function-class cl-ds.meta:put-back!-function))

(defgeneric put-front! (container item)
  (:generic-function-class cl-ds.meta:put-front!-function))

(defgeneric take-out! (container)
  (:generic-function-class cl-ds.meta:take-out!-function))

(defgeneric take-out (container)
  (:generic-function-class cl-ds.meta:functional-take-out-function))

(defgeneric take-out-back (container)
  (:generic-function-class cl-ds.meta:functional-take-out-back-function))

(defgeneric take-out-front (container)
  (:generic-function-class cl-ds.meta:functional-take-out-front-function))

(defgeneric take-out-back! (container)
  (:generic-function-class cl-ds.meta:take-out-back!-function))

(defgeneric take-out-front! (container)
  (:generic-function-class cl-ds.meta:take-out-front!-function))

(defgeneric near (container item maximal-distance))

(defgeneric add! (container location new-value)
  (:generic-function-class cl-ds.meta:add!-function))

(defgeneric insert (container location new-value)
  (:generic-function-class cl-ds.meta:functional-insert-function))

(defgeneric erase (container location)
  (:generic-function-class cl-ds.meta:functional-erase-function))

(defgeneric erase-if (container location condition-fn)
  (:generic-function-class cl-ds.meta:functional-erase-if-function))

(defgeneric erase-if! (container location condition-fn)
  (:generic-function-class cl-ds.meta:erase-if!-function))

(defgeneric erase! (container location)
  (:generic-function-class cl-ds.meta:erase!-function))

(defgeneric put! (container item)
  (:generic-function-class cl-ds.meta:put!-function))

(defgeneric size (container))

(defgeneric update (container location new-value)
  (:generic-function-class cl-ds.meta:functional-update-function))

(defgeneric update-if (container location new-value condition-fn)
  (:generic-function-class cl-ds.meta:functional-update-if-function))

(defgeneric update! (container location new-value)
  (:generic-function-class cl-ds.meta:update!-function))

(defgeneric update-if! (container location new-value condition-fn)
  (:generic-function-class cl-ds.meta:update-if!-function))

(defgeneric become-functional (container)
  (:method ((container functional)) container))

(defgeneric become-mutable (container)
  (:method ((container mutable)) container))

(defgeneric become-transactional (container))

(defgeneric become-lazy (container))

(defgeneric mutablep (container)
  (:method ((container mutable)) t)
  (:method ((container fundamental-container)) nil))

(defgeneric functionalp (container)
  (:method ((container functional)) t)
  (:method ((container fundamental-container)) nil))

(defgeneric transactionalp (container)
  (:method ((container transactional)) t)
  (:method ((container fundamental-container)) nil))

(defgeneric value (status))

(defgeneric found (status))

(defgeneric empty-clone (container))

(defgeneric traverse (function object)
  (:method (function (object sequence))
    (map nil function object))
  (:method (function (object fundamental-range))
    (iterate
      (for (values val more) = (cl-ds:consume-front object))
      (while more)
      (funcall function val))))

(defgeneric across (function object)
  (:method (function (object sequence))
    (map nil function object))
  (:method (function (object fundamental-container))
    (traverse function object))
  (:method (function (object fundamental-range))
    (traverse function (clone object))))

(defgeneric make-from-traversable (class traversable &rest arguments))

(defgeneric make-of-size (class size &rest more))

#|

Range releated functions.

|#

(defgeneric consume-front (range))

(defgeneric peek-front (range))

(defgeneric (setf peek-front) (new-value range))

(defgeneric consume-back (range))

(defgeneric peek-back (range))

(defgeneric (setf peek-back) (new-value range))

(defgeneric chunked (range &optional chunk-size-hint)
  (:method :before ((range fundamental-range) &optional chunk-size-hint)
    (check-type chunk-size-hint (or null positive-integer)))
  (:method ((range fundamental-range) &optional chunk-size-hint)
    (declare (ignore chunk-size-hint))
    nil))

(defgeneric dimensionality (object)
  (:method ((object fundamental-container))
    1)
  (:method ((object fundamental-range))
    1))

(defgeneric drop-front (range count)
  (:method ((range fundamental-forward-range) count)
    (check-type count non-negative-fixnum)
    (iterate
      (repeat count)
      (for i from 0)
      (for (values value more) = (consume-front range))
      (while more)
      (finally (return (values range i))))))

(defgeneric drop-back (range count)
  (:method ((range fundamental-bidirectional-range) count)
    (check-type count non-negative-fixnum)
    (iterate
      (repeat count)
      (for i from 0)
      (for (values value more) = (consume-back range))
      (while more)
      (finally (return (values range i))))))

(defgeneric clone (range))

(defgeneric whole-range (container)
  (:method ((range fundamental-range))
    range))

(defgeneric reset! (obj))

(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:functional-function))
  operation)

(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:erase!-function))
  #'erase)

(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:update-if!-function))
  #'update-if)

(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:erase-if!-function))
  #'erase-if)

(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:put!-function))
  #'put)

(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:add!-function))
  #'add)

(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:insert!-function))
  #'insert)

(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:take-out!-function))
  #'take-out)

(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:update!-function))
  #'update)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:take-out-back!-function))
  #'take-out-back)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:take-out-front!-function))
  #'take-out-front)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:put-front!-function))
  #'put-front)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:put-back!-function))
  #'put-back)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:destructive-function))
  operation)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-erase-function))
  #'erase!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-erase-if-function))
  #'erase-if!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-update-if-function))
  #'update-if!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-add-function))
  #'add!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-put-function))
  #'put!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-insert-function))
  #'(setf at))

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-take-out-function))
  #'take-out!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-take-out-back-function))
  #'take-out-back!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-take-out-front-function))
  #'take-out-front!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-put-front-function))
  #'put-front!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-put-back-function))
  #'put-back!)

(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-update-function))
  #'update!)

(defmethod clone ((range chunked-range))
  (make 'chunked-range
        :original-range (~> range read-original-range clone)
        :chunk-size (read-chunk-size range)))


(defmethod chunked ((range chunking-mixin) &optional chunk-size-hint)
  (make 'chunked-range
        :original-range (cl-ds:clone range)
        :chunk-size (or chunk-size-hint 256)))


(defmethod consume-front ((range chunked-range))
  (bind ((og-range (read-original-range range))
         ((:values item more) (consume-front og-range)))
    (if more
        (let* ((chunk-size (read-chunk-size range))
               (result (make-array chunk-size
                                   :adjustable t
                                   :fill-pointer 1)))
          (setf (aref result 0) item)
          (iterate
            (for i from 1 below chunk-size)
            (for (values elt m) = (consume-front og-range))
            (vector-push-extend elt result))
          (values (whole-range result)
                  t))
        (values nil nil))))


(defmethod peek-front ((range chunked-range))
  (bind ((og-range (~> range read-original-range cl-ds:clone))
         ((:values item more) (consume-front og-range)))
    (if more
        (let* ((chunk-size (read-chunk-size range))
               (result (make-array chunk-size
                                   :adjustable t
                                   :fill-pointer 1)))
          (setf (aref result 0) item)
          (iterate
            (for i from 1 below chunk-size)
            (for (values elt m) = (consume-front og-range))
            (vector-push-extend elt result))
          (values (whole-range result)
                  t))
        (values nil nil))))
