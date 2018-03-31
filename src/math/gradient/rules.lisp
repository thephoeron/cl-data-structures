(in-package #:cl-data-structures.math.gradient)


(define-tape-backward sin all
  (list (cos (car all))))


(define-tape-backward cos all
  (list (- (sin (car all)))))


(define-tape-backward + all
  (make-list (length all) :initial-element 1.0d0))


(define-tape-backward * all
  (reverse all))


(define-tape-backward expt all
  (bind (((x y . _) all))
    (* y (expt x (1- y)))))
