(in-package #:cl-data-structures)


(define-condition data-structure-condition ()
  ())


(define-condition argument-out-of-bounds (data-structure-condition)
  ())
