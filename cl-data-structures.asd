(in-package #:cl-user)


(asdf:defsystem cl-data-structures
  :name "cl-data-structures"
  :version "0.0.0"
  :license "MIT"
  :author "Lisp Mechanics"
  :maintainer "Lisp Mechanics"
  :depends-on ( :iterate         :alexandria
                :serapeum        :prove
                :prove-asdf      :documentation-utils-extensions
                :more-conditions :closer-mop
                :lparallel       :flexichain
                :metabang-bind   :bordeaux-threads
                :trivial-garbage :fare-memoization
                :scribble)
  :defsystem-depends-on (:prove-asdf)
  :serial T
  :pathname "src"
  :components ((:file "package")
               (:module "utils"
                :components ((:file "package")
                             (:file "macros")
                             (:file "types")
                             (:file "cartesian")
                             (:file "ordered-algorithms")
                             (:file "lists")
                             (:file "trivial")
                             (:file "modification-algorithms")
                             (:file "distances")
                             (:file "lazy-shuffle")
                             (:file "bind")
                             (:file "parallel-tools")
                             (:file "lambda-lists")
                             (:module "clustering"
                              :components ((:file "package")
                                           (:file "types")
                                           (:file "internal-functions")
                                           (:file "external-functions")
                                           (:file "docstrings")
                                           (:test-file "tests")))
                             (:test-file "distances-tests")
                             (:test-file "ordered-algorithms-tests")
                             (:test-file "lazy-shuffle-tests")))
               (:module "api"
                :components ((:file "meta")
                             (:file "meta-docstrings")
                             (:file "variables")
                             (:file "functions")
                             (:file "fundamental-classes")
                             (:file "trait-classes")
                             (:file "generics")
                             (:file "conditions")
                             (:file "expression-wrapper")
                             (:file "delay")
                             (:file "macros")
                             (:file "docstrings")
                             (:test-file "expression-tests")))
               (:module "adapters"
                :components ((:file "package")
                             (:file "hash-table")
                             (:file "vector")
                             (:file "list")
                             (:test-file "vector-tests")))
               (:module "common"
                :components ((:file "package")
                             (:file "modification-operation-status")
                             (:file "eager-modification-operation-status")
                             (:file "lazy-box")
                             (:file "lazy-range")
                             (:file "content-tuple")
                             (:file "ranges")
                             (:file "sequence-window")
                             (:test-file "sequence-window-tests")
                             (:module "abstract"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "hamt"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "rrb"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "egnat"
                              :components ((:file "package")
                                           (:file "classes")
                                           (:file "generics")
                                           (:file "common")
                                           (:file "methods")
                                           (:test-file "tests")))))
               (:module "algorithms"
                :components ((:file "package")
                             (:module "meta"
                              :components ((:file "macros")
                                           (:file "classes")
                                           (:file "generics")
                                           (:file "methods")
                                           (:file "docstrings")
                                           (:test-file "meta-tests")))
                             (:file "common")
                             (:file "on-each")
                             (:file "count")
                             (:file "summary")
                             (:file "to-vector")
                             (:file "common-range-category")
                             (:file "change-each!")
                             (:file "accumulate")
                             (:file "group-by")
                             (:file "without")
                             (:file "split-into-chunks")
                             (:file "hash-join")
                             (:file "chain")
                             (:file "zip")
                             (:file "docstrings")
                             (:test-file "split-into-chunks-test")
                             (:test-file "hash-join-tests")
                             (:test-file "without-test")
                             (:test-file "chain-tests")
                             (:test-file "zip-tests")))
               (:module "clustering"
                :components ((:file "package")
                             (:file "clara")))
               (:module "math"
                :components ((:file "package")
                             (:file "average")
                             (:file "variance")
                             (:file "mutual-information")
                             (:file "simple-linear-regression")
                             (:file "median-absolute-deviation")
                             (:file "hodges-lehmann")
                             (:file "standard-deviation")
                             (:file "moments")
                             (:file "statistical-summary")
                             (:file "bootstrap")
                             (:file "docstrings")
                             (:test-file "moments-tests")
                             (:test-file "statistical-summary-tests")
                             (:test-file "simple-linear-regression-tests")))
               (:module "streaming-algorithms"
                :components ((:file "package")
                             (:file "approximated-set-cardinality")
                             (:file "docstrings")))
               (:module "metric-space"
                :components ((:file "packages")
                             (:file "trait-classes")
                             (:file "common")
                             (:file "api")
                             (:module "egnat"
                              :components ((:file "api")))))
               (:module "dicts"
                :components ((:file "packages")
                             (:file "trait-classes")
                             (:file "common")
                             (:file "api")
                             (:file "docstrings")
                             (:module "hamt"
                              :components ((:file "api")
                                           (:file "docstrings")
                                           (:test-file "transactions-tests")
                                           (:test-file "range-test")
                                           (:test-file "lazy-tests")))
                             (:test-file "functional-dictionary-test-suite")
                             (:test-file "mutable-dictionary-test-suite")
                             (:test-file "transactional-dictionary-test-suite")))
               (:module "sequences"
                :components ((:file "packages")
                             (:file "common")
                             (:module "rrb"
                              :components ((:file "api")
                                           (:test-file "tests")))))
               (:module "data-frame"
                :components ((:file "packages")))))
