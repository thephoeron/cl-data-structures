(defpackage :cl-data-structures
  (:use :common-lisp)
  (:nicknames :cl-ds)
  (:export
   ;; generic functions
   :at
   :erase
   :erase!
   :add
   :add!
   :insert
   :empty-p
   :size
   :update
   :update!
   :become-functional
   :become-mutable
   :become-transactional
   :mutable-p
   :functional-p
   :transactional-p
   ;; trait classes
   :fundamental-container
   :functional
   :transactional
   :mutable))
