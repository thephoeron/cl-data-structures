(in-package #:cl-data-structures.utils.clustering)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function partition-around-medoids
    (:description "Clusters data set using partition-around-medoids algorithm. Requires precalculated distance matrix that will contain distance of each pair in the data set."
     :notes "Not well suited for clustering large data sets, as construction of full distance matrix will take large ammount of cpu cycles and memory."
     :thread-safety "Uses lparallel underneath."))

  (function clara
    (:description "Clusters data set using CLARA algorithm. This algorithm attempts to cluster random subset, picking best set of clusters."
     :notes "Useful for clustering large data sets, as required memory is linear to the size of data set and quadratic to the size of the sample."
     :returns ("Vector of vectors (each inner vector represents cluster)."
               "Silhouette of each cluster (as vector of single-floats).")
     :thread-safety "Uses lparallel underneath.")))
