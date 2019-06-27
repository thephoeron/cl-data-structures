(in-package #:cl-ds.utils)


(deftype extendable-vector (&optional (type t type-bound))
  `(and ,(if type-bound
             `(vector ,type)
             'vector)
     (not simple-array)
     (satisfies adjustable-array-p)
     (satisfies array-has-fill-pointer-p)))


(deftype index ()
  `(integer 0 ,ARRAY-TOTAL-SIZE-LIMIT))
