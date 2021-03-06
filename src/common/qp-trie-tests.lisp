(cl:in-package #:cl-data-structures.common.qp-trie)

(prove:plan 9)

(let ((tree (make 'qp-trie))
      (content nil)
      (bytes (make-array 5
                         :element-type '(unsigned-byte 8)
                         :initial-contents '(5 13 53 20 10))))
  (prove:is (qp-trie-find tree bytes) 0)
  (prove:ok (qp-trie-insert! tree bytes (make-qp-trie-node)))
  (prove:ok (not (qp-trie-insert! tree bytes (make-qp-trie-node))))
  (prove:is (qp-trie-find tree bytes) 5)
  (map-qp-trie-nodes (lambda (x)
                       (prove:is content nil)
                       (setf content x))
                     (access-root tree))
  (prove:is content bytes :test #'vector=)
  (prove:ok (qp-trie-delete! tree bytes))
  (prove:ok (not (qp-trie-delete! tree bytes)))
  (prove:is (qp-trie-find tree bytes) 0)
  )

(prove:finalize)
