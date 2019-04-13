(in-package #:cl-data-structures.file-system)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function line-by-line
    (:description "Opens a text file, and allows reading it line by line."
     :returns "Forward range. Provides access to each line in the file stored as a string."
     :exceptional-situations ("Signalling error from a callback during traverse will close the inner stream automaticly."
                              "Signals the type error if PATH is not (OR STRING PATHNAME)."
                              "Signals errors just as CL:OPEN does.")
     :notes "File is opened lazily. Calling either TRAVERSE or ACROSS on the line-by-line range will automaticly close the inner file. This makes it suitable to use with the aggregation functions without additional code."))

  (function find
    (:description "A function somewhat similar to the posix find tool. Depending on the filesystem content and the DESCRIPTION list, returns a forward-range containing pathnames matching the DESCRIPTION. DESCRIPTION list supports :directory, :regex-file, :regex-directory, :file, :all-directories and :all-files filters. Each of those filters selects matching nodes in the file system and handles it to the next filter."
     :notes ((:directory "This filter is used to specify directory by :PATH.")
             (:regex-directory "This filter is used to specify directory by PPCRE in the :PATH. Directories with a maching name will be accepted.")
             (:file "This filter is used to specify file by name supplied in :PATH.")
             (:regex-file "This filter is used to specify file by PPCRE in the :PATH. Files with a matching name will be accepted.")
             ))))
