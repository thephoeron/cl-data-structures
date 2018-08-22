(in-package #:cl-data-structures)


(defgeneric print-condition (object stream))


(define-condition textual-error (program-error)
  ((%text :initarg :text
          :type (or null string)
          :initform nil
          :reader read-text))
  (:report print-condition))


(defmethod print-condition :around ((condition textual-error) stream)
  (unless (null (read-text condition))
    (format stream "~a~%~%" (read-text condition)))
  (call-next-method))


(define-condition initialization-error (more-conditions:reference-condition
                                        textual-error)
  ((%class :initarg :class
           :reader read-class)))


(define-condition operation-not-allowed (more-conditions:reference-condition
                                         textual-error)
  ())


(define-condition ice-error (operation-not-allowed)
  ())


(defmethod print-condition ((condition initialization-error) stream)
  (format stream "During initialization of ~a...~%"
          (read-class condition))
  (call-next-method))


(define-condition invalid-argument (more-conditions:reference-condition
                                    textual-error)
  ((%argument :type symbol
              :initarg :argument
              :reader read-argument)))


(define-condition unexpected-argument (invalid-argument)
  ())


(define-condition incompatible-argument (invalid-argument)
  ())


(define-condition out-of-bounds (textual-error)
  ((%value :initarg :value
           :reader read-value)
   (%bounds :initarg :bounds
            :reader read-bounds)))


(define-condition argument-out-of-bounds (invalid-argument
                                          out-of-bounds)
  ())


(define-condition not-in-allowed-set (out-of-bounds)
  ())


(define-condition argument-not-in-allowed-set (not-in-allowed-set
                                               invalid-argument)
  ())


(define-condition empty-container (textual-error)
  ())


(define-condition dimensionality-error (out-of-bounds)
  ())


(define-condition too-many-dimensions (dimensionality-error)
  ())


(defmethod print-condition ((condition dimensionality-error) stream)
  (format stream "Dimensionality is ~a." (read-bounds condition)))


(defmethod print-condition ((condition argument-out-of-bounds) stream)
  (if (slot-boundp condition '%argument)
      (format stream "Argument ~A has value ~a which is out of bounds ~a.~%"
              (read-argument condition)
              (read-value condition)
              (read-bounds condition))
      (format stream "Value ~a is out of bounds ~a.~%"
              (read-value condition)
              (read-bounds condition))))


(defmethod print-condition ((condition not-in-allowed-set) stream)
  (let ((value (read-value condition))
        (bounds (read-bounds condition))
        (i 20))
    (block out
      (format stream "Value ~a is not in the set: " value)
      (map nil
           (lambda (x)
             (if (zerop (decf i))
                 (return-from out nil)
                 (format stream "~A " x)))
           bounds))
    (unless (zerop i)
      (format stream "(...)"))
    (format stream "~%"))
  (call-next-method))


(defmethod print-condition ((condition invalid-argument) stream)
  (format stream "Invalid argument ~a."
          (read-argument condition)))


(defmethod print-condition ((condition unexpected-argument) stream)
  (format stream "Argument ~A was not expected.~%"
          (read-argument condition))
  (call-next-method))


(define-condition initialization-out-of-bounds (initialization-error
                                                argument-out-of-bounds)
  ())


(define-condition not-implemented (program-error)
  ())
