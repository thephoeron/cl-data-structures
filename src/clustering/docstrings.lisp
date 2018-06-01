(in-package #:cl-data-structures.clustering)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function clara
    (:description "Clusters data set using CLARA algorithm. This algorithm attempts to cluster random subset, picking best set of clusters."
     :notes "Useful for clustering large data sets, as required memory is linear to the size of data set and quadratic to the size of the sample."
     :arguments ((range "Data to cluster.")
                 (number-of-medoids "How many clusters should be created?")
                 (sample-size "Size of sample used to cluster data.")
                 (sample-count "Number of samples that will be drawn.")
                 (metric-fn "Distance function used for clustering.")
                 (metric-type "Type returned by METRIC-FN.")
                 (key "Function used to extract value for metric-fn.")
                 (select-medoids-attempts-count "How many times PAM should attempt to select medoids before accepting suboptimal medoids?")
                 (attempts "How many times clusters should be splitted and merged before accepting suboptimal cluster sizes.")
                 (split "Threshold size of clusters. Clusters above this size will attempt to be splitted.")
                 (merge "Threshold size of clusters. Clusters below this size will attempt to be merged."))
     :returns ("Vector of vectors (each inner vector represents cluster)."
               "Silhouette of each cluster (as vector of single-floats).")
     :thread-safety "Uses lparallel underneath.")))
