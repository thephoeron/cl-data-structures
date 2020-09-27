(in-package :cl-user)

(defpackage cl-data-structures/asdf
  (:use cl asdf uiop))

(in-package :cl-data-structures/asdf)

(defsystem cl-data-structures
  :name "cl-data-structures"
  :version "1.0.0"
  :license "BSD simplified"
  :author "Lisp Mechanics"
  :maintainer "Lisp Mechanics"
  :depends-on (iterate
               alexandria
               serapeum
               documentation-utils-extensions
               more-conditions
               closer-mop
               lparallel
               flexichain
               metabang-bind
               bordeaux-threads
               scribble
               osicat
               cl-fad
               cl-progress-bar
               trivial-garbage
               cl-ppcre)
  :serial t
  :pathname "src"
  :components ((:file "aux-package")
               (:file "package")
               (:module "utils"
                :components ((:file "package")
                             (:file "macros")
                             (:file "types")
                             (:file "higher-order")
                             (:file "cartesian")
                             (:file "ordered-algorithms")
                             (:file "lists")
                             (:file "trivial")
                             (:file "modification-algorithms")
                             (:file "distances")
                             (:file "lazy-shuffle")
                             (:file "arrays")
                             (:file "trees")
                             (:file "bind")
                             (:file "parallel-tools")
                             (:file "lambda-lists")
                             (:file "skip-vector")
                             (:file "embedding")
                             (:file "cloning")
                             (:file "numbers")
                             (:file "bucket-sort")
                             (:file "hashing")
                             (:file "docstrings")
                             (:module "metric-functions"
                              :components ((:file "package")
                                           (:file "levenshtein")
                                           (:file "hellinger")
                                           (:file "average-metric")
                                           (:file "lcs-metric")
                                           (:file "hausdorff")
                                           (:file "euclid")
                                           (:file "earth-mover")
                                           (:file "svr")
                                           (:file "docstrings")))
                             (:module "distance-functions"
                              :components ((:file "package")
                                           (:file "sinkhorn")
                                           (:file "bhattacharyya")
                                           (:file "docstrings")))
                             (:module "clustering"
                              :components ((:file "package")
                                           (:file "common")
                                           (:module "k-means"
                                            :components ((:file "package")
                                                         (:file "types")
                                                         (:file "internal")
                                                         (:file "external")))
                                           (:module "clara-pam"
                                            :components ((:file "package")
                                                         (:file "types")
                                                         (:file "internal")
                                                         (:file "external")
                                                         (:file "docstrings")))
                                           (:module "bubble"
                                            :components ((:file "package")
                                                         (:file "cf-tree-protocol")
                                                         (:file "cf-tree-structure")
                                                         (:file "cf-tree-implementation")
                                                         (:file "cf-tree-algorithm")
                                                         (:file "api")))))))
               (:module "api"
                :components ((:file "meta")
                             (:file "meta-docstrings")
                             (:file "fundamental-classes")
                             (:file "trait-classes")
                             (:file "generics")
                             (:file "conditions")
                             (:file "expression-wrapper")
                             (:file "delay")
                             (:file "macros")
                             (:file "functions")
                             (:file "field")
                             (:file "ranges")
                             (:file "docstrings")))
               (:module "adapters"
                :components ((:file "package")
                             (:file "hash-table")
                             (:file "vector")
                             (:file "list")))
               (:module "common"
                :components ((:file "package")
                             (:file "modification-operation-status")
                             (:file "eager-modification-operation-status")
                             (:file "lazy-box")
                             (:file "lazy-range")
                             (:file "content-tuple")
                             (:file "ranges")
                             (:file "sequence-window")
                             (:file "docstrings")
                             (:module "abstract"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "2-3-tree"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "hamt"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "rrb"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "skip-list"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "egnat"
                              :components ((:file "package")
                                           (:file "classes")
                                           (:file "generics")
                                           (:file "common")
                                           (:file "methods")
                                           (:file "docstrings")))
                             (:file "qp-trie")))
               (:module "dicts"
                :components ((:file "packages")
                             (:file "trait-classes")
                             (:file "common")
                             (:file "api")
                             (:file "docstrings")
                             (:module "hamt"
                              :components ((:file "api")
                                           (:file "docstrings")))
                             (:module "srrb"
                              :components ((:file "types")
                                           (:file "internal")
                                           (:file "api")))))
               (:module "sequences"
                :components ((:file "packages")
                             (:file "common")
                             (:module "rrb"
                              :components ((:file "api")
                                           (:file "docstrings")))))
               (:module "queues"
                :components ((:file "packages")
                             (:file "common")
                             (:file "docstrings")
                             (:module "2-3-tree"
                              :components ((:file "api")))))
               (:module "sets"
                :components ((:file "packages")
                             (:file "common")
                             (:module "qp-trie"
                              :components ((:file "api")
                                           (:file "docstrings")))
                             (:module "skip-list"
                              :components ((:file "api")))))
               (:module "metric-space"
                :components ((:file "packages")
                             (:file "trait-classes")
                             (:file "common")
                             (:file "api")
                             (:file "docstrings")
                             (:module "egnat"
                              :components ((:file "api")))))
               (:module "algorithms"
                :components ((:file "package")
                             (:module "meta"
                              :components ((:file "macros")
                                           (:file "classes")
                                           (:file "generics")
                                           (:file "methods")
                                           (:file "docstrings")))
                             (:file "common")
                             (:file "array-elementwise")
                             (:file "on-each")
                             (:file "translation")
                             (:file "count")
                             (:file "to-vector")
                             (:file "to-list")
                             (:file "rate")
                             (:file "to-hash-table")
                             (:file "enumerate")
                             (:file "shuffled-range")
                             (:file "filtering")
                             (:file "common-range-category")
                             (:file "summary")
                             (:file "change-each!")
                             (:file "accumulate")
                             (:file "group-by")
                             (:file "without")
                             (:file "multiplex")
                             (:file "only")
                             (:file "cartesian")
                             (:file "restrain-size")
                             (:file "reservoir-sample")
                             (:file "repeat")
                             (:file "latch")
                             (:file "extrema")
                             (:file "extremum")
                             (:file "cumulative-accumulate")
                             (:file "split-into-chunks")
                             ;; (:file "hash-join")
                             (:file "chain")
                             (:file "frequency")
                             (:file "zip")
                             (:file "flatten-lists")
                             (:file "partition-if")
                             (:file "distinct")
                             (:file "first-element")
                             (:file "docstrings")))
               (:module "file-system"
                :components ((:file "package")
                             (:file "common")
                             (:file "line-by-line")
                             (:file "tokenize")
                             (:file "find")
                             (:file "unix")
                             (:file "docstrings")))
               (:module "threads"
                :components ((:file "package")
                             (:file "parallel-multiplex")
                             (:file "parallel-group-by")
                             (:file "parallel-on-each")
                             (:file "buffer-range")
                             (:file "docstrings")))
               (:module "clustering"
                :components ((:file "package")
                             (:file "clara")
                             (:file "k-means")
                             (:file "docstrings")))
               (:module math
                :components ((:module gamma
                              :components ((:file "package")
                                           (:file "gamma")))
                             (:file "package")
                             (:file "absolute-value-norm")
                             (:file "average")
                             (:file "variance")
                             (:file "mutual-information")
                             (:file "simple-linear-regression")
                             (:file "median-absolute-deviation")
                             (:file "hodges-lehmann")
                             (:file "co-occurence-table")
                             (:file "standard-deviation")
                             (:file "moments")
                             ;; (:file "chi-squared")
                             (:file "bootstrap")
                             (:file "moving-average")
                             (:file "hmm")
                             (:file "gini-impurity")
                             (:file "entropy")
                             (:file "fast-map")
                             (:file "sum")
                             (:file "docstrings")))
               (:module "streaming-algorithms"
                :components ((:file "hyperloglog")
                             (:file "polynomial-hashing")
                             (:file "package")
                             (:file "common")
                             (:file "approximated-set-cardinality")
                             (:file "approximated-counts")
                             (:file "approximated-top-k")
                             (:file "bloom-filter")
                             (:file "minhash")
                             (:file "docstrings")))))
