(cl:in-package #:cl-data-structures.file-system)


(defclass command ()
  ((%command-string :type string
                    :initarg :command-string
                    :reader read-command-string)))


(defmethod stream-designator-p ((designator command))
  t)


(defmethod open-stream-designator ((designator command))
  (~> designator read-command-string
      (uiop:launch-program :output :stream :force-shell t)
      uiop/launch-program:process-info-output))


(defun command (command)
  (check-type command string)
  (make 'command :command-string command))
