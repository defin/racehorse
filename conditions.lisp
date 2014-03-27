;;;; -*- Mode: Lisp; Package: KIWI -*-

(in-package racehorse)

;;; Conditions

(define-condition resource-condition (condition)
  ((resource :initarg :resource
	     :reader resource-error-resource)))

(define-condition resource-over-soft-limit (resource-condition) ())

(define-condition resource-error (error)
  ((resource :initarg :resource
	     :reader resource-error-resource)))

(define-condition resource-constructor-function-missing (resource-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Resource ~A has no constructor function."
                     (resource-name (resource-error-resource condition))))))

(define-condition resource-over-hard-limit (resource-error)
  ()
  (:report (lambda (condition stream)
	     (let ((resource (resource-error-resource condition)))
	       (format stream "Allocating another object would cause resource ~A to exceed its hard limit of ~D."
		       (resource-name resource) (resource-hard-limit resource))))))

(define-condition resource-does-not-exist (resource-error)
  ((name 
    :initarg :name
    :reader resource-error-name))
  (:report (lambda (condition stream)
             (format stream "No resource named ~A."
                     (resource-error-name condition)))))

(define-condition object-not-owned-by-resource (resource-error)
  ((object
    :initarg :object
    :reader resource-error-object))
  (:report (lambda (condition stream)
             (format stream "Attempt to release object ~S, which does not belong to resource ~A."
                     (resource-error-object condition) (resource-name (resource-error-resource condition))))))

(define-condition object-already-released (resource-error)
  ((object
    :initarg :object
    :reader resource-error-object))
  (:report (lambda (condition stream)
             (format stream "Attempt to release object ~S, which has already been released."
                     (resource-error-object condition)))))
