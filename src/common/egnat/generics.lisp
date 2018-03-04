(in-package #:cl-data-structures.common.egnat)


(defgeneric bucket-head (container bucket)
  (:method ((container fundamental-egnat-container)
            (bucket vector))
    (assert (not (emptyp bucket)))
    (aref bucket 0)))
