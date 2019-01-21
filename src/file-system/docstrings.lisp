(in-package #:cl-data-structures.file-system)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function line-by-line
    (:description "Opens text file, and allows reading it line by line by consuming returned range."
     :returns "Forward range."
     :exceptional-situations ("Signalling error from callback during traverse will close the inner stream."
                              "Signals errors just as CL:OPEN does.")
     :notes "Care has been taken to ensure that not to many sockets will be open at the same time."))

  (function find
    (:description "Function that is somewhat similar to posix find tool. Depending on the current filesystem and the DESCRIPTION list, it will return forward-range containing pathnames matching DESCRIPTION.")))
