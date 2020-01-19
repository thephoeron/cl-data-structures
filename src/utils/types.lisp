(cl:in-package #:cl-ds.utils)


(deftype extendable-vector (&optional (type '*))
  `(and (vector ,type)
        (satisfies adjustable-array-p)
        (satisfies array-has-fill-pointer-p)))


(deftype index ()
  `(integer 0 ,ARRAY-TOTAL-SIZE-LIMIT))
