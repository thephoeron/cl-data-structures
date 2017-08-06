(in-package #:cl-data-structures)


(define-condition data-structure-condition (error)
  ((%text :initarg :text
          :type string
          :reader read-text)))


(defmethod print-object :around ((condition data-structure-condition) stream)
  (format stream "~a~%~%" (read-text condition))
  (call-next-method))


(define-condition initialization-error (more-conditions:reference-condition
                                        data-structure-condition)
  ((%class :initarg :class
           :reader read-class)))


(defmethod print-object ((condition initialization-error) stream)
  (format stream "During initialization of ~a...~%"
          (read-class condition))
  (call-next-method))


(define-condition invalid-argument (data-structure-condition)
  ())


(define-condition argument-out-of-bounds (more-conditions:reference-condition
                                          invalid-argument)
  ())


(define-condition initialization-out-of-bounds (initialization-error
                                                argument-out-of-bounds)
  ())


(define-condition not-implemented (data-structure-condition)
  ())

