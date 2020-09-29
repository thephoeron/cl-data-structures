(in-package :cl-user)

(defpackage cl-data-structures.file-system
  (:nicknames cl-ds.fs)
  (:use c2cl cl-data-structures.aux-package)
  (:shadow #:find)
  (:export #:close-inner-stream
           #:find
           #:line-by-line
           #:file-range-mixin
           #:close-stream
           #:open-stream-designator
           #:stream-designator-p
           #:access-reached-end
           #:access-current-position
           #:ensure-stream
           #:tokenize
           #:command
           #:read-path
           #:with-file-ranges))
