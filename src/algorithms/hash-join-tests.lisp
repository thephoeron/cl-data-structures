(in-package #:cl-user)
(defpackage hash-join-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:hash-join-tests)

(plan 9)
(let ((result (sort (cl-ds.alg:hash-join #(1 2 3 4) #'identity
                                         (list (cl-ds:field nil
                                                            :data #(1 2 3)
                                                            :key #'identity)))
                    #'<
                    :key #'first)))
  (iterate
    (for v in-vector result)
    (for (a b) = v)
    (is a b)))
(ok (emptyp (cl-ds.alg:hash-join #() #'identity '((#() . identity)))))
(ok (emptyp (cl-ds.alg:hash-join #() #'identity '((#(1 2 3) . identity)))))
(ok (emptyp (cl-ds.alg:hash-join #(1 2 3) #'identity '((#() . identity)))))
(iterate
  (for (elt) in-vector (sort (cl-ds.alg:hash-join #(1 2 3) #'identity '())
                             #'< :key #'car))
  (for i from 1)
  (is elt i))
(finalize)
