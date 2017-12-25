(in-package #:cl-user)


(asdf:defsystem cl-data-structures
  :name "cl-data-structures"
  :version "0.0.0"
  :license "MIT"
  :author "Lisp Mechanics"
  :maintainer "Lisp Mechanics"
  :depends-on ( :iterate :alexandria
                :serapeum :prove
                :prove-asdf
                :docstample :more-conditions
                :closer-mop :lparallel
                :flexichain)
  :defsystem-depends-on (:prove-asdf)
  :serial T
  :pathname "src"
  :components ((:file "package")
               (:module "utils"
                :components ((:file "package")
                             (:file "macros")
                             (:file "types")
                             (:file "ordered-algorithms")
                             (:file "lists")
                             (:file "trivial")
                             (:file "modification-algorithms")
                             (:file "distances")
                             (:file "lazy-shuffle")
                             (:test-file "distances-tests")
                             (:test-file "ordered-algorithms-tests")
                             (:test-file "lazy-shuffle-tests")))
               (:module "api"
                :components ((:file "meta")
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
               (:module "algorithms"
                :components ((:file "package")
                             (:file "meta")
                             (:file "common")
                             (:file "on-each")
                             (:file "summary")
                             (:file "change-each!")
                             (:file "accumulate")
                             (:file "group-by")))
               (:module "stat"
                :components ((:file "package")
                             (:file "average")
                             (:file "entropy")))
               (:module "common"
                :components ((:file "package")
                             (:file "modification-operation-status")
                             (:file "eager-modification-operation-status")
                             (:file "lazy-box")
                             (:file "lazy-range")
                             (:file "content-tuple")
                             (:file "ranges")
                             (:module "hamt"
                              :components ((:file "package")
                                           (:file "common")))))
               (:module "adapters"
                :components ((:file "package")
                             (:file "hash-table")))
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
                             (:test-file "transactional-dictionary-test-suite")))))
