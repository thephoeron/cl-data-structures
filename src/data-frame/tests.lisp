(in-package #:cl-user)

(defpackage data-frame-tests
  (:use #:cl #:prove #:serapeum #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:data-frame-tests)

(plan 53)

(let ((frame (make 'cl-ds.df:data-frame
                   :upper-bounds (~> '(5 3)
                                     (coerce '(vector non-negative-fixnum)))
                   :dimensionality 2)))
  (#2=is-error (cl-ds:at frame -1 -1) #1='cl-ds:argument-out-of-bounds)
  (#2# (cl-ds:at frame -1 2) #1#)
  (#2# (cl-ds:at frame 1 -1) #1#)
  (#2# (cl-ds:at frame 5 3) #1#)
  (#2# (cl-ds:at frame 5 2) #1#)
  (#2# (cl-ds:at frame 2 3) #1#)
  (#2# (cl-ds:at frame 1 1 1) #3='cl-ds:dimensionality-error)
  (#2# (cl-ds:at frame 1) #3#)
  (#2# (cl-ds:at frame nil nil) #4='type-error)
  (#2# (cl-ds:at frame 0.2 0.5) #4#))

(let ((frame (cl-ds.df:stack 1 #(1 2 3) #(4 5 6))))
  (is (cl-ds:dimensionality frame) 2)
  (is-error (cl-ds:at frame 0 0 0) 'cl-ds:dimensionality-error)
  (is-error (cl-ds:at frame 9 0) 'cl-ds:argument-out-of-bounds)
  (is (cl-ds:at frame 0 0) 1)
  (is (cl-ds:at frame 1 0) 2)
  (is (cl-ds:at frame 2 0) 3)
  (is (cl-ds:at frame 0 1) 4)
  (is (cl-ds:at frame 1 1) 5)
  (is (cl-ds:at frame 2 1) 6)
  (is (cl-ds:size frame) 6)
  (let ((i 0))
    (cl-ds.df:mutate! frame
                      1
                      (lambda () (iterate
                              (for j from 0 below 3)
                              (is (cl-ds.df:cell j) (incf i)))))))

(let ((frame (cl-ds.df:stack 0 #(1 2 3) #(3 4 5))))
  (is (cl-ds:dimensionality frame) 2)
  (is-error (cl-ds:at frame 0 0 0) 'cl-ds:dimensionality-error)
  (is-error (cl-ds:at frame 9 0) 'cl-ds:argument-out-of-bounds)
  (is (cl-ds:at frame 0 0) 1)
  (is (cl-ds:at frame 0 1) 2)
  (is (cl-ds:at frame 0 2) 3)
  (is (cl-ds:at frame 1 0) 3)
  (is (cl-ds:at frame 1 1) 4)
  (is (cl-ds:at frame 1 2) 5))

(let ((frame (cl-ds.df:stack 0 #(1 2 3) #(4 5 6) #(7 8 9))))
  (is (cl-ds:dimensionality frame) 2)
  (is-error (cl-ds:at frame 0 0 0) 'cl-ds:dimensionality-error)
  (is-error (cl-ds:at frame 9 0) 'cl-ds:argument-out-of-bounds)
  (is (cl-ds:at frame 0 0) 1)
  (is (cl-ds:at frame 0 1) 2)
  (is (cl-ds:at frame 0 2) 3)
  (is (cl-ds:at frame 1 0) 4)
  (is (cl-ds:at frame 1 1) 5)
  (is (cl-ds:at frame 1 2) 6)
  (is (cl-ds:at frame 2 0) 7)
  (is (cl-ds:at frame 2 1) 8)
  (is (cl-ds:at frame 2 2) 9))

(is-error (cl-ds.df:stack 0 #(1 2 3) #(3 4 5 6))
          'cl-ds:initialization-error)

(is-error (cl-ds.df:stack 1 #(1 2 3) #(3 4 5 6))
          'cl-ds:initialization-error)

(is-error (cl-ds.df:stack 2 #(1 2 3) #(3 4 5))
          'cl-ds:argument-out-of-bounds)

(is-error (cl-ds.df:stack -1 #(1 2 3) #(3 4 5))
          'cl-ds:argument-out-of-bounds)

(is-error (cl-ds.df:stack -1 #(1 2 3) #(3 4 5))
          'cl-ds:argument-out-of-bounds)

(is-error (cl-ds.df:stack 0.2 #(1 2 3) #(3 4 5))
          'cl:type-error)

(finalize)
