(cl:in-package #:cl-data-structures.utils)


(define-symbol-macro todo
    (error 'cl-ds:not-implemented :format-control "Not implemented"))
