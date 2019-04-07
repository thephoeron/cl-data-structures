(in-package #:cl-data-structures.clustering)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function k-means
    (:description "Clusters data set using k-means algorithm. Data points must be represented as one dimensional simple-arrays specialized for single-floats."
     :exceptional-situations "Will signal a TYPE-ERROR when key does not return (SIMPLE-ARRAY 'SINGLE-FLOAT (*))."
     :returns "Clusters object. Clusters itself are represented as a vector of vectors and can be read using CLUSTER-CONTENTS function. Additionally, silhouette values can be read with SILHOUETTE function."
     :notes "This algorithm does not calculate silhouette during it's operation and because of that calling silhouette on the result will take extra time."
     :thread-safety "Uses lparallel underneath."))

  (function clara
    (:description "Clusters data set using CLARA algorithm. This algorithm attempts to cluster random subset, picking the best set of clusters."
     :notes ("Useful for clustering large data sets, as required memory is linear to the size of the data set and quadratic to the size of the sample."
             "Algorithm initially described in the Clustering Large Applications article.")
     :arguments ((range "Data to cluster.")
                 (number-of-medoids "How many clusters should be created?")
                 (sample-size "Size of sample used to cluster data.")
                 (sample-count "Number of samples that will be drawn.")
                 (metric-fn "Distance function used for clustering.")
                 (metric-type "Type returned by METRIC-FN.")
                 (key "Function used to extract value for metric-fn.")
                 (select-medoids-attempts-count "How many times PAM should attempt to select medoids before accepting suboptimal medoids?")
                 (attempts "How many times clusters should be splitted and merged before accepting suboptimal cluster sizes.")
                 (split "Threshold size of clusters. Function will attempt to split clusters above this size.")
                 (merge "Threshold size of clusters. Function will attempt to merge clusters below this size."))
     :returns "Clusters object. Clusters itself are represented as a vector of vectors and can be read using CLUSTER-CONTENTS function. Additionally, silhouette values can be read with SILHOUETTE function."
     :thread-safety "Uses lparallel underneath.")))
