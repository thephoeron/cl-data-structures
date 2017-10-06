(in-package #:cl-data-structures.documentation)


(cl-lore.api.syntax:syntax
 cl-lore.extensions.documentation.protocol
 cl-lore.extensions.documentation.api
 cl-lore.extensions.sequence-graphs.api)


(cl-lore.api.syntax:define-save-output-function save-output
    (with-names (<documentation-names>)
      (document (cl-lore.mechanics:<mechanics-html-output-generator> out *cl-data-structures*
                 :output-options (:css cl-lore.mechanics:*mechanics-html-style*))
        @title{CL-DATA-STRUCTURES}
        @include{cl-ds intro}
        @include{cl-ds API}
        @include{dicts})))
