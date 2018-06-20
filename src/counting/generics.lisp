(in-package #:cl-data-structures.counting)


(defgeneric association-frequency (set))

(defgeneric association-information-gain (set))

(defgeneric type-count (object))

(defgeneric find-association (index apriori aposteriori))

(defgeneric total-entropy (object))

(defgeneric all-super-sets (set))

(defgeneric apriori-set (set))

(defgeneric aposteriori-set (set))

(defgeneric content (set))

(defgeneric make-apriori-set (apriori aposteriori))

(defgeneric support (object))
