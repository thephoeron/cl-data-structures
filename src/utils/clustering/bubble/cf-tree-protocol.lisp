(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defgeneric cf-insert (cf-tree node item))
(defgeneric make-subtree (cf-tree))
(defgeneric make-leaf (cf-tree))
(defgeneric absorb-nodes (cf-tree parent new-children
                          &optional old-child))
(defgeneric needs-split-p (cf-tree node))
(defgeneric split (cf-tree node))
(defgeneric clusteroid-distance (cf-tree first-node second-node-or-item))
(defgeneric needs-resampling-p (cf-tree node))
(defgeneric resample (cf-tree subtree))
(defgeneric clusteroid (cf-tree node))
(defgeneric average-distance (cf-tree first-node-or-item second-node-or-item))
