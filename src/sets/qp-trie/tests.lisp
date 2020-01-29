(cl:in-package #:cl-data-structures.sets.qp-trie)

(prove:plan 501)

(let ((trie (make-mutable-qp-trie-set))
      (data (make-array 500)))
  (map-into data (lambda () (map-into (make-array 4 :element-type '(unsigned-byte 8))
                                      (lambda () (random #.(expt 2 8))))))
  (iterate
    (for vect in-vector data)
    (cl-ds:put! trie vect))
  (iterate
    (for vect in-vector data)
    (prove:ok (cl-ds:at trie vect)))
  (prove:is (cl-ds.alg:count-elements trie)
            (~> data (remove-duplicates :test #'vector=) length))
  )

(prove:finalize)
