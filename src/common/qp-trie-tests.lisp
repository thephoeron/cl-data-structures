(cl:in-package #:cl-data-structures.qp-trie)


(prove:plan 4)

(let ((tree (make 'qp-trie))
      (bytes (make-array 5
                         :element-type '(unsigned-byte 8)
                         :initial-contents '(5 13 53 20 10))))
  (prove:is (qp-trie-find tree bytes) 0)
  (prove:ok (qp-trie-insert! tree bytes) )
  (prove:ok (not (qp-trie-insert! tree bytes)))
  (prove:is (qp-trie-find tree bytes) 5))

(prove:finalize)
