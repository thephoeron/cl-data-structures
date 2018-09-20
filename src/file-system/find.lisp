(in-package #:cl-data-structures.file-system)


(defclass find-range (cl-ds:chunking-mixin
                      cl-ds:fundamental-forward-range)
  ((%stack :initarg :stack
           :initform nil
           :accessor access-stack)))


(defclass fundamental-file-range-stack-cell ()
  ((%prev-cell :initarg :prev-cell
               :initform nil
               :type fundamental-file-range-stack-cell
               :accessor access-prev-cell)
   (%path :initarg :path
          :type (or nil string pathname)
          :reader read-path)))


(defclass stateful-file-range-stack-cell ()
  ((%state :initform nil
           :accessor access-state
           :type list
           :initarg :state)
   (%initial-state :initform nil
                   :accessor access-initial-state
                   :type list
                   :initarg :initial-state)))


(defclass directory-file-range-stack-cell (fundamental-file-range-stack-cell)
  ())


(defclass recursive-content-file-range-stack-cell (stateful-file-range-stack-cell
                                                   directory-file-range-stack-cell)
  ())


(defclass file-file-range-stack-cell (fundamental-file-range-stack-cell)
  ())


(defclass all-files-file-range-stack-cell (stateful-file-range-stack-cell
                                           file-file-range-stack-cell)
  ())


(defclass regex-file-file-range-stack-cell (stateful-file-range-stack-cell
                                            file-file-range-stack-cell)
  ())


(defclass regex-directory-file-range-stack-cell (stateful-file-range-stack-cell
                                                 directory-file-range-stack-cell)
  ((%times :initarg :times
           :reader read-times))
  (:initargs (:times 1)))


(defmethod (setf access-prev-cell)
    :before ((new-val all-files-file-range-stack-cell)
             (cell regex-directory-file-range-stack-cell))
  (error 'cl-ds:initialization-error
         :text "Directory can't be stacked on top of file in the path description."
         :class 'find-range))


(defmethod (setf access-prev-cell)
    :before ((new-val file-file-range-stack-cell)
             (cell directory-file-range-stack-cell))
  (error 'cl-ds:initialization-error
         :text "Directory can't be stacked on top of file in the path description."
         :class 'find-range))


(defmethod (setf access-prev-cell)
    :before ((new-val (eql nil))
             (cell regex-directory-file-range-stack-cell))
  (error 'cl-ds:initialization-error
         :text "Regex form can't occur as first in the path description."
         :class 'find-range))


(defmethod (setf access-prev-cell)
    :before ((new-val (eql nil))
             (cell file-file-range-stack-cell))
  (error 'cl-ds:initialization-error
         :text "Files form can't occur as first in the path description."
         :class 'find-range))


(defgeneric make-stack-cell (name &key &allow-other-keys))


(defmethod make-instance :before ((range regex-directory-file-range-stack-cell)
                                  &key path times)
  (check-type path string)
  (check-type times (or list positive-integer symbol))
  (if (listp times)
      (progn
        (check-type (first times) positive-integer)
        (check-type (second times) (or positive-integer symbol))
        (unless (eql (length times) 2)
          (error 'cl-ds:invalid-argument
                 :argument :times
                 :text "Times list should contain lower bound and upper bound."))
        (if (symbolp (second times))
            (unless (eql :recursive (second times))
              (error 'cl-ds:not-in-allowed-set
                     :value (second times)
                     :bounds '(:recursive)
                     :text "Upper bound is supposed to be either integer or :recursive symbol."))
            (unless (apply #'<= times)
              (error 'cl-ds:invalid-argument
                     :argument :times
                     :text "Lower bound of times should be less then upper bound."))))
      (check-type times non-negative-integer)))


(defmethod make-instance :before ((range file-file-range-stack-cell)
                                  &key path)
  (check-type path (or string pathname)))


(defmethod make-instance :before ((range directory-file-range-stack-cell)
                                  &key path)
  (check-type path (or string pathname)))


(defmethod make-instance :before ((range recursive-content-file-range-stack-cell)
                                  &key path)
  (check-type path (or string pathname)))


(defmethod make-instance :before ((range regex-file-file-range-stack-cell)
                                  &key path)
  (check-type path string))


(defmethod make-stack-cell ((name (eql :directory)) &key path)
  (make 'directory-file-range-stack-cell
        :path path))


(defmethod make-stack-cell ((name (eql :regex-directory)) &key path (times 1))
  (make 'regex-directory-file-range-stack-cell
        :path path
        :times times))


(defmethod make-stack-cell ((name (eql :regex-file)) &key path)
  (make 'regex-file-file-range-stack-cell
        :path path))


(defmethod make-stack-cell ((name (eql :file)) &key path)
  (make 'file-file-range-stack-cell
        :path path))


(defmethod make-stack-cell ((name (eql :all-directories)) &key (path ""))
  (make 'recursive-content-file-range-stack-cell
        :path path))


(defmethod make-stack-cell ((name (eql :all-files)) &key)
  (make 'all-files-file-range-stack-cell
        :path nil))


(defun to-pathname (x &rest args)
  (etypecase x
    (string (apply #'make-pathname args))
    (pathname x)))


(defmethod cl-ds:consume-front ((cell directory-file-range-stack-cell))
  (tagbody :start
     (if (~> cell access-prev-cell null)
         (let* ((path (read-path cell)))
           (setf (slot-value cell '%path) nil)
           (if (and (not (null path))
                    (setf path (to-pathname path :directory path))
                    (osicat:directory-exists-p path))
               (return-from cl-ds:consume-front (values path t))
               (return-from cl-ds:consume-front (values nil nil))))
         (let ((prev-path (~> cell access-prev-cell cl-ds:consume-front)))
           (if (null prev-path)
               (return-from cl-ds:consume-front (values nil nil))
               (let ((result (merge-pathnames (read-path cell) prev-path)))
                 (if (osicat:directory-exists-p result)
                     (return-from cl-ds:consume-front (values result t))
                     (go :start))))))))


(defmethod cl-ds:consume-front ((cell recursive-content-file-range-stack-cell))
  (tagbody :start
     (when (~> cell access-state first (eql :end))
       (return-from cl-ds:consume-front
         (values nil nil)))
     (bind ((path (read-path cell)))
       (if (null (access-state cell))
           (let ((prev-path nil))
             (if (~> cell access-prev-cell null)
                 (progn
                   (setf prev-path (to-pathname path :directory path))
                   (unless (cl-fad:directory-exists-p prev-path)
                     (push :end (access-state cell))
                     (return-from cl-ds:consume-front
                       (values nil nil))))
                 (if-let ((p (~>> cell access-prev-cell cl-ds:consume-front)))
                   (progn
                     (setf prev-path (merge-pathnames path p))
                     (unless (cl-fad:directory-exists-p prev-path)
                       (push :end (access-state cell))
                       (return-from cl-ds:consume-front
                         (values nil nil))))
                   (progn
                     (push :end (access-state cell))
                     (return-from cl-ds:consume-front
                       (values nil nil)))))
             (setf (access-state cell) (list prev-path))
             (go :start))
           (iterate
             (until (or (null (access-state cell))
                        (~> cell access-state first (eql :end))))
             (for next-path = (pop (access-state cell)))
             (for directory-content = (~>> next-path directory-content
                                           (delete-if-not #'cl-fad::directory-exists-p)))
             (setf (access-state cell)
                   (cl-ds.utils:add-to-list directory-content
                                            (access-state cell)))
             (when (and (~> cell access-state endp)
                        (null (access-prev-cell cell)))
               (push :end (access-state cell)))
             (return-from cl-ds:consume-front
               (values next-path t))
             (finally (if (null (access-prev-cell cell))
                          (progn
                            (push :end (access-end cell))
                            (return-from cl-ds:consume-front
                              (values nil nil)))
                          (go :start))))))))


(defmethod cl-ds:consume-front ((cell all-files-file-range-stack-cell))
  (tagbody :start
     (bind ((state (access-state cell)))
       (if (endp state)
           (let ((prev-path (~> cell access-prev-cell cl-ds:consume-front)))
             (if (null prev-path)
                 (return-from cl-ds:consume-front
                   (values nil nil))
                 (let* ((directory-content
                          (~>> prev-path directory-content
                               (delete-if-not (rcurry #'osicat:file-exists-p
                                                      :regular-file)))))
                   (setf (access-state cell) directory-content)
                   (go :start))))
           (iterate
             (until (endp (access-state cell)))
             (for next-path = (pop (access-state cell)))
             (return-from cl-ds:consume-front
               (values next-path t))
             (finally (go :start)))))))


(defmethod cl-ds:consume-front ((cell regex-file-file-range-stack-cell))
  (tagbody :start
     (bind ((path (read-path cell))
            (state (access-state cell)))
       (if (null state)
           (let ((prev-path (~> cell access-prev-cell cl-ds:consume-front)))
             (if (null prev-path)
                 (return-from cl-ds:consume-front
                   (values nil nil))
                 (let* ((directory-content
                          (~>> prev-path directory-content
                               (delete-if-not (rcurry #'osicat:file-exists-p
                                                      :regular-file)))))
                   (setf (access-state cell) directory-content)
                   (go :start))))
           (iterate
             (until (endp (access-state cell)))
             (for next-path = (pop (access-state cell)))
             (when (regex-matches path next-path)
               (return-from cl-ds:consume-front
                 (values next-path t)))
             (finally (go :start)))))))


(defun directory-content (directory)
  (cl-fad:list-directory directory))


(defun directory-regex-matches (regex path parent)
  (~> (osicat:unmerge-pathnames path parent)
      namestring
      (cl-ppcre:scan regex _)
      not
      null))


(defun regex-matches (regex path)
  (~> path
      (osicat:unmerge-pathnames (cl-fad:pathname-parent-directory path))
      namestring
      (cl-ppcre:scan regex _)
      not
      null))


(defun not-greater (times depth)
  (if (listp times)
      (or (eql (second times) :recursive)
          (<= depth (second times)))
      (if (eql times :recursive)
          t
          (<= depth times))))


(defun times-matches (times depth)
  (if (listp times)
      (and (>= depth (first times))
           (or (eql (second times) :recursive)
               (< depth (second times))))
      (if (eql times :recursive)
          t
          (= depth times))))


(defmethod cl-ds:consume-front ((cell regex-directory-file-range-stack-cell))
  (tagbody :start
     (bind ((path (read-path cell))
            (state (access-state cell))
            (times (read-times cell)))
       (if (null state)
           (let ((prev-path (~> cell access-prev-cell cl-ds:consume-front)))
             (if (null prev-path)
                 (return-from cl-ds:consume-front
                   (values nil nil))
                 (let* ((directory-content (~>> prev-path directory-content
                                                (delete-if-not #'cl-fad:directory-exists-p)))
                        (new-state (mapcar (curry #'list 1 prev-path) directory-content)))
                   (setf (access-state cell) new-state)
                   (go :start))))
           (iterate
             (until (endp (access-state cell)))
             (for (depth prev-path next-path) = (pop (access-state cell)))
             (when (not-greater times depth)
               (let ((directory-content (~>> next-path directory-content
                                             (delete-if-not #'cl-fad:directory-exists-p))))
                 (setf (access-state cell)
                       (cl-ds.utils:add-to-list (mapcar (curry #'list (1+ depth) prev-path)
                                                        directory-content)
                                                (access-state cell)))
                 (when (and (times-matches times depth)
                            (directory-regex-matches path next-path prev-path))
                   (return-from cl-ds:consume-front
                     (values next-path t)))))
             (finally (go :start)))))))


(defmethod cl-ds:consume-front ((cell file-file-range-stack-cell))
  (bind ((path (read-path cell))
         (inner (~> cell access-prev-cell cl-ds:consume-front)))
    (if (null inner)
        (values nil nil)
        (let ((next-path (merge-pathnames path inner)))
          (if (osicat:file-exists-p next-path :regular-file)
              (values next-path t)
              (values nil nil))))))


(defgeneric find (description)
  (:method ((description list))
    (let ((result nil))
      (iterate
        (for spec in description)
        (for stack-cell = (apply #'make-stack-cell spec))
        (setf (access-prev-cell stack-cell) result
              result stack-cell))
      (make 'find-range
            :stack result))))


(defmethod cl-ds:consume-front ((range find-range))
  (cl-ds:consume-front (access-stack range)))


(defmethod cl-ds:clone ((range find-range))
  (make 'find-range
        :stack (cl-ds:clone (access-stack range))))


(defmethod cl-ds:clone :around ((cell stateful-file-range-stack-cell))
  (lret ((result (call-next-method)))
    (setf (access-state result) (access-state cell)
          (access-initial-state result) (access-state cell))))


(defmethod cl-ds:clone ((cell regex-directory-file-range-stack-cell))
  (lret ((result (call-next-method)))
    (setf (slot-value result '%times) (read-times cell))))


(defmethod cl-ds:clone ((cell fundamental-file-range-stack-cell))
  (make (type-of cell)
        :prev-cell (and #1=(access-prev-cell cell) (cl-ds:clone #1#))
        :path (read-path cell)))


(defmethod cl-ds:peek-front ((range find-range))
  (cl-ds:peek-front (cl-ds:clone range)))


(defmethod cl-ds:reset! ((range find-range))
  (cl-ds:reset! (access-stack range))
  range)


(defmethod cl-ds:reset! ((cell stateful-file-range-stack-cell))
  (setf (access-state cell) (access-initial-state cell))
  (call-next-method))


(defmethod cl-ds:reset! ((cell fundamental-file-range-stack-cell))
  (when #1=(access-prev-cell cell) (cl-ds:reset! #1#)))
