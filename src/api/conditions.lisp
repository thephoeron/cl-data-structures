(in-package #:cl-data-structures)


(define-condition textual-error (error)
  ((%text :initarg :text
          :type (or null string)
          :initform nil
          :reader read-text)))


(defmethod print-object :around ((condition textual-error) stream)
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


(defmethod print-object ((condition initialization-error) stream)
  (format stream "During initialization of ~a...~%"
          (read-class condition))
  (call-next-method))


(define-condition invalid-argument (more-conditions:reference-condition
                                    textual-error)
  ((%argument :type symbol
              :initarg :argument
              :reader read-argument)))


(define-condition out-of-bounds (textual-error)
  ((%value :initarg :value
           :reader read-value)
   (%bounds :initarg :bounds
            :reader read-bounds)))


(define-condition argument-out-of-bounds (invalid-argument
                                          out-of-bounds)
  ())


(define-condition empty-container (textual-error)
  ())


(define-condition dimensionality-error (textual-error)
  ())


(define-condition too-many-dimensions (dimensionality-error)
  ())


(defmethod print-object ((condition argument-out-of-bounds) stream)
  (format stream "Argument ~a has value ~a which is out of bounds ~a~%"
          (read-argument condition)
          (read-value condition)
          (read-bounds condition))
  (call-next-method))


(define-condition initialization-out-of-bounds (initialization-error
                                                argument-out-of-bounds)
  ())


(define-condition not-implemented (error)
  ())
