(defpackage :cl-data-structures.documentation
  (:use #:cl))


(in-package #:cl-data-structures.documentation)


(cl-lore.api.syntax:syntax
 cl-lore.extensions.documentation.protocol
 cl-lore.extensions.documentation.api
 cl-lore.extensions.sequence-graphs.api)


(cl-lore.api.syntax:define-save-output-function
    build-docs
    (cl-lore.mechanics:<mechanics-html-output-generator>
     *cl-data-structures*
     :output-options (:css cl-lore.mechanics:*mechanics-html-style*))
    ("manual.lisp")
    (with-names (<documentation-names>)
      @title{CL-DATA-STRUCTURES}
      @include{cl-ds intro}
      @include{cl-ds API}
      @include{dicts}))
