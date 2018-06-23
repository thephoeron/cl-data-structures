(in-package #:cl-data-structures.counting)


(defgeneric association-frequency (set))

(defgeneric type-count (object))

(defgeneric find-association (index apriori aposteriori))

(defgeneric find-set (index &rest content))

(defgeneric all-super-sets (set minimal-frequency &optional maximal-size))

(defgeneric all-sets (index minimal-frequency &optional maximal-size))

(defgeneric apriori-set (set))

(defgeneric aposteriori-set (set))

(defgeneric content (set))

(defgeneric make-apriori-set (apriori aposteriori))

(defgeneric support (object))
