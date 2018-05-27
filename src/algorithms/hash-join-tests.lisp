(in-package #:cl-user)
(defpackage hash-join-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:hash-join-tests)

(plan 6)
(ok (emptyp (cl-ds.alg:hash-join #() #'identity
                                 (list (cl-ds:field :data #()
                                                    :key #'identity)))))
(ok (emptyp (cl-ds.alg:hash-join #() #'identity
                                 (list (cl-ds:field :data #(1 2 3)
                                                    :key #'identity)))))
(ok (emptyp (cl-ds.alg:hash-join #(1 2 3) #'identity
                                 (list (cl-ds:field :data #()
                                                    :key #'identity)))))
(iterate
  (for (elt) in-vector (sort (cl-ds.alg:hash-join #(1 2 3) #'identity nil)
                             #'< :key #'car))
  (for i from 1)
  (is elt i))
(finalize)
