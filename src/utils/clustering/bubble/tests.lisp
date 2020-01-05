(cl:in-package #:cl-user)
(defpackage :bubble-test-suite (:use
                                :cl :alexandria
                                :cl-data-structures.utils.clustering.bubble))
(in-package :bubble-test-suite)


(defun metric (a b)
  (abs (- a b)))


(let* ((data (coerce (shuffle (iota 10000)) 'vector))
       (bubbles (bubble-grouping
                 data
                 #'metric
                 5
                 10
                 10
                 10
                 3)))
  (prove:is (every (lambda (bubble)
                     (let* ((clusteroid (bubble-clusteroid bubble))
                            (content (bubble-content bubble))
                            (distance-sum (reduce #'+
                                                  :key (lambda (x)
                                                         (metric x clusteroid))))
                            (radius (sqrt (/ distance-sum (length content)))))
                       (<= radius 3)))
                   bubbles))
  )
