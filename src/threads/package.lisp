(in-package :cl-user)

(defpackage :cl-data-structures.threads
  (:use c2cl cl-data-structures.aux-package)
  (:nicknames cl-ds.threads)
  (:export #:thread-buffer
           #:parallel-multiplex
           #:parallel-on-each
           #:parallel-group-by))
