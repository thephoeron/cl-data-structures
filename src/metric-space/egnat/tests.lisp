(in-package :cl-user)
(defpackage metric-egnat-tests
  (:use :prove :cl :iterate))
(in-package :metric-egnat-tests)

(plan 43)

(let* ((path (asdf:system-relative-pathname :cl-data-structures "test/files/words.txt"))
       (data (serapeum:vect))
       (count 0))
  (with-open-file (stream path)
    (iterate
      (for word = (read-line stream nil nil))
      (until (null word))
      (vector-push-extend word data)))
  (let ((set (cl-ds:make-from-traversable
              'cl-ds.ms.egnat:mutable-egnat-metric-set
              data
              #'string=
              (lambda (a b)
                (incf count)
                (cl-ds.utils.metric:levenshtein-metric a b))
              'non-negative-fixnum)))
    (setf count 0)
    (iterate
      (with near = (cl-ds:near set "rose" 1))
      (for n = (cl-ds:consume-front near))
      (until (null n))
      (for distance = (cl-ds.utils.metric:levenshtein-metric n "rose"))
      (ok (<= distance 1))
      (count t into result)
      (finally (is result 40)))
    (ok (null (zerop count)))
    (ok (< count (length data)))))

(finalize)
