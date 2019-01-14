(in-package #:cl-data-structures.algorithms)


(defclass distinct-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%seen :initarg :seen
          :reader read-seen)
   (%mutex :initarg :mutex
           :reader read-mutex)
   (%original-seen :initarg :original-seen
                   :reader read-original-seen))
  (:default-initargs :mutex (bt:make-lock)))


(defclass forward-distinct-proxy (cl-ds:chunking-mixin
                                  distinct-proxy
                                  fundamental-forward-range)
  ())


(defmethod cl-ds:clone ((range distinct-proxy))
  (bt:with-lock-held ((read-mutex range))
    (let* ((seen (read-seen range))
           (original-seen (read-seen range))
           (new-seen1 (cl-ds:become-transactional seen))
           (new-seen2 (cl-ds:become-transactional seen)))
      ;; below is required to prevent spilling out side effects on seen dict between clones.
      (setf (slot-value range '%seen) new-seen1)
      (make (type-of range)
            :key (read-key range)
            :seen new-seen2
            :original-seen original-seen
            :original-range (read-original-range range)))))


(defmethod cl-ds:drop-front ((range distinct-proxy) count)
  (iterate
    (repeat count)
    (iterate
      (with mutex = (read-mutex range))
      (with range = (read-original-range range))
      (with key = (read-key range))
      (for (values data more) = (cl-ds:consume-front range))
      (unless more
        (return-from drop-front (values nil nil)))
      (for key-value = (funcall key data))
      (bt:with-lock-held (mutex)
        (cl-ds:mod-bind (dict found) (cl-ds:add! (read-seen range)
                                                 key-value t)
          (unless found
            (leave (values data t)))))))
  range)


(defmethod cl-ds:traverse ((range distinct-proxy) function)
  (bind (((:slots %seen %mutex %key) range)
         (original (read-original-range range)))
    (cl-ds:traverse
     original
     (lambda (x &aux (key (funcall %key x)))
       (bt:with-lock-held (%mutex)
         (cl-ds:mod-bind (dict found) (cl-ds:add! %seen key t)
           (unless found
             (funcall function x)))))))
  range)


(defmethod cl-ds:across ((range distinct-proxy) function)
  (bind (((:slots %seen %key %mutex) range)
         (original (read-original-range range))
         (seen (cl-ds:become-transactional %seen)))
    (cl-ds:across
     original
     (lambda (x &aux (key (funcall %key x)))
       (bt:with-lock-held (%mutex)
         (cl-ds:mod-bind (dict found) (cl-ds:add! seen key t)
           (unless found
             (funcall function x)))))))
  range)


(defmethod cl-ds:reset! ((range distinct-proxy))
  (bt:with-lock-held ((read-mutex range))
    (setf (slot-value range '%seen) (~> range
                                        read-seen
                                        cl-ds:become-transactional)))
  (call-next-method))


(defmethod cl-ds:peek-front ((range forward-distinct-proxy))
  (iterate
    (with mutex = (read-mutex range))
    (with seen = (bt:with-lock-held (mutex)
                   (cl-ds:become-transactional (read-seen range))))
    (with range = (cl-ds:clone (read-original-range range)))
    (with key = (read-key range))
    (for (values data more) = (cl-ds:consume-front seen))
    (unless more
      (leave (values nil nil)))
    (for key-value = (funcall key data))
    (cl-ds:mod-bind (dict found) (cl-ds:add! seen key-value t)
      (unless found
        (leave (values data t))))))


(defmethod cl-ds:consume-front ((range forward-distinct-proxy))
  (iterate
    (with mutex = (read-mutex range))
    (with range = (read-original-range range))
    (with key = (read-key range))
    (for (values data more) = (cl-ds:consume-front range))
    (unless more
      (leave (values nil nil)))
    (for key-value = (funcall key data))
    (bt:with-lock-held (mutex)
      (cl-ds:mod-bind (dict found) (cl-ds:add! (read-seen range)
                                               key-value t)
        (unless found
          (leave (values data t)))))))


(defclass distinct-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric distinct (range &key key test hash-function)
  (:generic-function-class distinct-function)
  (:method (range &key (key #'identity) (test 'eql) (hash-function #'sxhash))
    (ensure-functionf key hash-function)
    (apply-range-function range #'distinct :key key :test test
                                           :hash-function hash-function)))


(defmethod apply-layer ((range fundamental-forward-range)
                        (function distinct-function)
                        &rest all &key key test hash-function)
  (declare (ignore all))
  (make 'forward-distinct-proxy
        :key key
        :seen (cl-ds.dicts.hamt:make-mutable-hamt-dictionary hash-function
                                                             test)
        :original-range range))
