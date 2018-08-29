(in-package #:cl-data-structures.algorithms)


(defclass distinct-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%seen :initarg :seen
          :reader read-seen)
   (%original-seen :initarg :original-seen
                   :reader read-original-seen)))


(defclass forward-distinct-proxy (distinct-proxy
                                  fundamental-forward-range)
  ())


(defmethod cl-ds:clone ((range distinct-proxy))
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
          :original-range (read-original-range range))))


(defmethod cl-ds:traverse (function (range distinct-proxy))
  (bind (((:slots %seen %key) range)
         (original (read-original-range range)))
    (cl-ds:traverse
     (lambda (x)
       (cl-ds:mod-bind (dict found) (cl-ds:add! %seen (funcall %key x) t)
         (unless found
           (funcall function x))))
     original))
  range)


(defmethod cl-ds:across (function (range distinct-proxy))
  (bind (((:slots %seen %key) range)
         (original (read-original-range range))
         (seen (cl-ds:become-transactional %seen)))
    (cl-ds:across
     (lambda (x)
       (cl-ds:mod-bind (dict found) (cl-ds:add! seen (funcall %key x) t)
         (unless found
           (funcall function x))))
     original))
  range)


(defmethod cl-ds:reset! ((range distinct-proxy))
  (setf (slot-value range '%seen) (cl-ds:become-transactional (read-seen range)))
  (call-next-method))


(defmethod cl-ds:peek-front ((range forward-distinct-proxy))
  (iterate
    (with seen = (cl-ds:become-transactional (read-seen range)))
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
    (with seen = (read-seen range))
    (with range = (read-original-range range))
    (with key = (read-key range))
    (for (values data more) = (cl-ds:consume-front seen))
    (unless more
      (leave (values nil nil)))
    (for key-value = (funcall key data))
    (cl-ds:mod-bind (dict found) (cl-ds:add! seen key-value t)
      (unless found
        (leave (values data t))))))


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
