(in-package :cl-user)
(defpackage :reconstruct-data-from-subtree
  (:use :cl :prove :serapeum :iterate :alexandria :cl-ds.utils)
  (:shadowing-import-from :iterate :collecting :summing :in))
(in-package :reconstruct-data-from-subtree)

(import-all-package-symbols :cl-ds.dicts.hamt :reconstruct-data-from-subtree)

(progn
  (diag "Testing reconstructing HAMT nodes")
  (prove:plan 8)
  (diag "Reconstructing when only one leaf is present as child in hash node should turn hash node into conflict")
  (let* ((conflict (make-conflict-node (list (make-hash.location.value :location 'a :value 'b))))
         (hamt-node (make-hash-node :leaf-mask #b100
                                    :content (make-array 1 :initial-element conflict)))
         (rec-node (reconstruct-data-from-subtree hamt-node 8)))
    (ok rec-node)
    (ok (typep rec-node 'conflict-node))
    (is (hash.location.value-location (car (access-conflict rec-node))) 'a))
  (diag "Reconstructing when two leafs are present in hash node should move one of the conflict tuples to hash node")
  (let* ((tuple1 (make-hash.location.value :location 'a :value 'b))
         (tuple2 (make-hash.location.value :location 'c :value 'd))
         (conflict1 (make-conflict-node (list tuple1)))
         (conflict2 (make-conflict-node (list tuple2)))
         (hamt-node (make-hash-node :leaf-mask #b101
                                    :content (make-array 2 :initial-contents (list conflict1 conflict2))))
         (rec-node (reconstruct-data-from-subtree hamt-node 8)))
    (ok rec-node)
    (ok (hash-node-p rec-node))
    (is (hash-node-leaf-mask rec-node) #b100)
    (is (hash-node-data rec-node) tuple1)
    (is (car (access-conflict (hash-node-access rec-node 2)))
        tuple2))
  (finalize))
