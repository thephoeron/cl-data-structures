(in-package #:cl-data-structures.file-system)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function line-by-line
    (:description "Opens text file, and allows reading it line by line by consuming returned range."
     :returns "Forward range."
     :exceptional-situations ("Signalling error from callback during traverse will close the inner stream."
                              "Signals errors just as CL:OPEN does.")
     :notes "Traversing range will close inner stream.")))
