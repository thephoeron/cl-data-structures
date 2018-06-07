(in-package #:cl-user)

(defpackage data-frame-tests
  (:use #:cl #:prove #:serapeum #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:data-frame-tests)

(plan 6)

(let ((frame (make-instance 'cl-ds.df:data-frame
                            :sizes (~> '(5 3)
                                       (coerce '(vector non-negative-fixnum)))
                            :dimensionality 2)))
  (#2=is-error (cl-ds:at frame -1 -1) #1='cl-ds:argument-out-of-bounds)
  (#2# (cl-ds:at frame -1 2) #1#)
  (#2# (cl-ds:at frame 1 -1) #1#)
  (#2# (cl-ds:at frame 5 3) #1#)
  (#2# (cl-ds:at frame 5 2) #1#)
  (#2# (cl-ds:at frame 2 3) #1#))

(finalize)
