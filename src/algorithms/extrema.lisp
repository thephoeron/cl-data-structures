(in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    extrema extrema-function

  (:range fn &key key value-key)
  (:range fn &key (key #'identity) (value-key #'identity))

  (%low-value %high-value %fn %first-iteration %value-key)

  ((ensure-functionf value-key fn)
   (setf %fn fn
         %value-key value-key
         %first-iteration t))

  ((element)
   (cl-ds.utils:lazy-let ((elt (funcall %value-key element)))
     (cond (%first-iteration
            (setf %low-value element
                  %high-value element
                  %first-iteration nil))
           ((funcall %fn
                     elt
                     (funcall %value-key %high-value))
            (setf %high-value element))
           ((funcall %fn
                     (funcall %value-key %low-value)
                     elt)
            (setf %low-value element)))))

  ((list* %high-value %low-value)))
