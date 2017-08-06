(in-package #:cl-data-structures)


(define-condition data-structure-condition (error)
  ((%text :initarg :text
          :type (or null string)
          :initform nil
          :reader read-text)))


(defmethod print-object :around ((condition data-structure-condition) stream)
  (unless (null (read-text condition))
    (format stream "~a~%~%" (read-text condition)))
  (call-next-method))


(define-condition initialization-error (more-conditions:reference-condition
                                        data-structure-condition)
  ((%class :initarg :class
           :reader read-class)))


(defmethod print-object ((condition initialization-error) stream)
  (format stream "During initialization of ~a...~%"
          (read-class condition))
  (call-next-method))


(define-condition invalid-argument (more-conditions:reference-condition
                                    data-structure-condition)
  ((%argument :type symbol
              :initarg :argument
              :reader read-argument)))


(define-condition out-of-bounds (data-structure-condition)
  ((%value :initarg :value
           :reader read-value)
   (%bounds :initarg :bounds
            :reader read-bounds)))


(define-condition argument-out-of-bounds (invalid-argument
                                          out-of-bounds)
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

