(in-package #:cl-user)


(asdf:defsystem cl-data-structures
  :name "cl-data-structures"
  :version "0.0.0"
  :license "MIT"
  :author "Lisp Mechanics"
  :maintainer "Lisp Mechanics"
  :depends-on ( :iterate :alexandria
                :serapeum :prove
                :docstample :more-conditions
                :closer-mop)
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
                             (:file "modification-algorithms")))
               (:module "api"
                :components ((:file "meta")
                             (:file "fundamental-classes")
                             (:file "trait-classes")
                             (:file "generics")
                             (:file "conditions")
                             (:file "macros")
                             (:file "docstrings")))
               (:module "common"
                :components ((:file "package")
                             (:file "modification-operation-status")
                             (:file "eager-modification-operation-status")
                             (:file "lazy-box")))
               (:module "dicts"
                :components ((:file "packages")
                             (:file "trait-classes")
                             (:file "docstrings")
                             (:module "hamt"
                              :components ((:file "internal")
                                           (:file "api")
                                           (:file "docstrings")))))))
