(cl:in-package #:cl-data-structures.algorithms)


(defclass abstract-partition-if-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%on-first :initarg :on-first
              :reader read-on-first)
   (%test :initarg :test
          :reader read-test)))


(defclass forward-abstract-partition-if-proxy
    (abstract-partition-if-proxy
     fundamental-forward-range)
  ())


(defclass bidirectional-abstract-partition-if-proxy
    (forward-abstract-partition-if-proxy
     fundamental-bidirectional-range)
  ())


(defclass random-access-abstract-parition-if-proxy
    (bidirectional-abstract-partition-if-proxy
     fundamental-random-access-range)
  ())


(defclass callback-partition-if-proxy ()
  ((%callback :reader read-callback
              :initarg :callback)))


(defclass forward-callback-partition-if-proxy
    (callback-partition-if-proxy forward-abstract-partition-if-proxy)
  ())


(defclass bidirectional-callback-partition-if-proxy
    (callback-partition-if-proxy bidirectional-abstract-partition-if-proxy)
  ())


(defclass random-access-callback-parition-if-proxy
    (callback-partition-if-proxy random-access-abstract-parition-if-proxy)
  ())


(defclass partition-if-proxy ()
  ((%chunks :initform (vect)
            :reader read-chunks)))


(defclass forward-partition-if-proxy
    (partition-if-proxy forward-abstract-partition-if-proxy)
  ())


(defclass bidirectional-partition-if-proxy
    (partition-if-proxy bidirectional-abstract-partition-if-proxy)
  ())


(defclass random-access-parition-if-proxy
    (partition-if-proxy random-access-abstract-parition-if-proxy)
  ())


(defmethod clone ((range callback-partition-if-proxy))
  (make (type-of range)
        :callback (read-callback range)
        :on-first (read-on-first range)
        :original-range (~> range read-original-range clone)
        :key (read-key range)
        :test (read-test range)))


(defmethod clone ((range abstract-partition-if-proxy))
  (make (type-of range)
        :original-range (~> range read-original-range clone)
        :on-first (read-on-first range)
        :key (read-key range)
        :test (read-test range)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range callback-partition-if-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (bind ((on-first (read-on-first range))
         (test (ensure-function (read-test range)))
         (partition-key (ensure-function (read-key range)))
         (callback (ensure-function (read-callback range)))
         (outer-fn (call-next-method)))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.utils:cases ((:variant (eq partition-key #'identity))
                         (:variant (eq test #'eq)
                                   (eq test #'eql)
                                   (eq test #'equal)
                                   (eq test #'string=)
                                   (eq test #'=)
                                   (eq test #'equalp)))
       (cl-ds.alg.meta:let-aggregator ((initialized nil)
                                       (current-key nil)
                                       (current-state nil))
           ((element)
             (bind ((key (~>> element (funcall partition-key))))
               (cond ((not initialized)
                      (setf current-key key
                            initialized t
                            current-state (cl-ds.alg.meta:call-constructor outer-fn))
                      (pass-to-aggregation current-state element))
                     ((funcall test current-key key)
                      (unless on-first
                        (setf current-key key))
                      (pass-to-aggregation current-state element))
                     (t
                      (~>> current-state
                           cl-ds.alg.meta:extract-result
                           (funcall callback))
                      (setf current-key key
                            current-state (cl-ds.alg.meta:call-constructor outer-fn))
                      (pass-to-aggregation current-state element)))))

           (nil)))
     function
     arguments)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range partition-if-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (bind ((on-first (read-on-first range))
         (test (ensure-function (read-test range)))
         (partition-key (ensure-function (read-key range)))
         (outer-fn (call-next-method)))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.utils:cases ((:variant (eq partition-key #'identity))
                         (:variant (eq test #'eq)
                                   (eq test #'eql)
                                   (eq test #'equal)
                                   (eq test #'string=)
                                   (eq test #'=)
                                   (eq test #'equalp)))
       (cl-ds.alg.meta:let-aggregator
           ((chunks (the (cl-ds.utils:extendable-vector t) (vect))))

           ((element)
             (let* ((chunks-length (fill-pointer chunks))
                    (last-chunk (the fixnum (1- chunks-length)))
                    (key (funcall partition-key element))
                    (empty (zerop chunks-length)))
               (if empty
                   (let ((new (cl-ds.alg.meta:call-constructor outer-fn)))
                     (vector-push-extend (list* key new)
                                         chunks)
                     (cl-ds.alg.meta:pass-to-aggregation new element))
                   (bind (((prev . chunk) (aref chunks last-chunk)))
                     (if (funcall test prev key)
                         (pass-to-aggregation chunk element)
                         (let ((new (cl-ds.alg.meta:call-constructor outer-fn)))
                           (vector-push-extend (list* key new)
                                               chunks)
                           (cl-ds.alg.meta:pass-to-aggregation new element)))
                     (unless on-first
                       (setf (car (aref chunks (1- (fill-pointer chunks)))) key))))))

           ((~> (cl-ds.utils:transform (compose #'cl-ds.alg.meta:extract-result #'cdr)
                                       chunks)
                cl-ds:whole-range))))

     function
     arguments)))


(defclass partition-if-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass callback-partition-if-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric partition-if-with-callback (range test callback &key key on-first)
  (:generic-function-class callback-partition-if-function)
  (:method (range test callback &key (key #'identity) (on-first nil))
    (ensure-functionf test key callback)
    (apply-range-function range #'partition-if-with-callback
                          (list range test callback
                                :key key
                                :on-first on-first))))


(defgeneric partition-if (range test &key key on-first)
  (:generic-function-class partition-if-function)
  (:method (range test &key (key #'identity) (on-first nil))
    (ensure-functionf test key)
    (apply-range-function range #'partition-if
                          (list range test
                                :key key
                                :on-first on-first))))


(defmethod apply-layer ((range traversable)
                        (fn partition-if-function)
                        all)
  (make-proxy range 'forward-partition-if-proxy
              :key (cl-ds.utils:at-list all :key)
              :on-first (cl-ds.utils:at-list all :on-first)
              :test (second all)))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn partition-if-function)
                        all)
  (make-proxy range 'bidirectional-partition-if-proxy
              :key (cl-ds.utils:at-list all :key)
              :on-first (cl-ds.utils:at-list all :on-first)
              :test (second all)))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn partition-if-function)
                        all)
  (make-proxy range 'random-access-parition-if-proxy
              :key (cl-ds.utils:at-list all :key)
              :on-first (cl-ds.utils:at-list all :on-first)
              :test (second all)))


(defmethod apply-layer ((range traversable)
                        (fn callback-partition-if-function)
                        all)
  (make-proxy range 'forward-callback-partition-if-proxy
              :key (cl-ds.utils:at-list all :key)
              :on-first (cl-ds.utils:at-list all :on-first)
              :callback (third all)
              :test (second all)))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn callback-partition-if-function)
                        all)
  (make-proxy range 'bidirectional-callback-partition-if-proxy
              :key (cl-ds.utils:at-list all :key)
              :on-first (cl-ds.utils:at-list all :on-first)
              :callback (third all)
              :test (second all)))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn callback-partition-if-function)
                        all)
  (make-proxy range 'random-access-callback-parition-if-proxy
              :key (cl-ds.utils:at-list all :key)
              :on-first (cl-ds.utils:at-list all :on-first)
              :callback (third all)
              :test (second all)))
