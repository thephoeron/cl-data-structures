(defpackage :cl-data-structures.documentation
  (:use #:cl #:cl-lore
        #:cl-lore.api.syntax
        #:cl-lore.extensions.documentation.api
        #:cl-lore.extensions.sequence-graphs.api))


(in-package #:cl-data-structures.documentation)

(cl-lore.api.syntax:syntax
 cl-lore.extensions.documentation.api
 cl-lore.extensions.sequence-graphs.api)

(cl-lore.api.syntax:define-save-output-function
    build-docs
    (:cl-data-structures.documentation
     (<documentation-names>)
     cl-lore.mechanics:<mechanics-html-output-generator>
     *cl-data-structures*)
    (:output-options (:css cl-lore.mechanics:*mechanics-html-style*))

  ("vars.lisp"
   "manual.lisp"
   "in-depth.lisp")

  @title{CL-DATA-STRUCTURES}
  @include{cl-ds intro}
  @include{cl-ds API}
  @include{cl-ds internals}
  @include{dicts})
