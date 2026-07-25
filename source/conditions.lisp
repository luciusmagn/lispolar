(in-package #:lispolar)

;;;; -- Conditions --

(define-condition polar-error (error)
  ((message
    :initarg  :message
    :reader   polar-error-message
    :type     string
    :documentation "Human-readable description of the failure.")
   (operation
    :initarg  :operation
    :reader   polar-error-operation
    :type     keyword
    :documentation "The client operation that failed.")
   (cause
    :initarg  :cause
    :initform nil
    :reader   polar-error-cause
    :type     t
    :documentation "Underlying condition when available."))
  (:report
   (lambda (condition stream)
     (format stream "~A" (polar-error-message condition))))
  (:documentation "Base condition for Polar client failures."))


(define-condition polar-http-error (polar-error)
  ((status
    :initarg  :status
    :reader   polar-error-status
    :type     integer
    :documentation "HTTP status returned by Polar.")
   (body
    :initarg  :body
    :reader   polar-error-body
    :type     string
    :documentation "Raw response body returned by Polar."))
  (:report
   (lambda (condition stream)
     (format stream "~A (HTTP ~A)"
             (polar-error-message condition)
             (polar-error-status condition))))
  (:documentation "Polar rejected or failed an HTTP request."))


(define-condition polar-webhook-error (polar-error)
  ()
  (:documentation "Webhook signature or header validation failed."))


(define-condition polar-parse-error (polar-error)
  ()
  (:documentation "A Polar payload could not be parsed."))


(defun polar--fail (operation message &optional cause)
  "Signal a POLAR-ERROR for OPERATION."
  (error 'polar-error
         :message   message
         :operation operation
         :cause     cause))


(defun polar--http-fail (operation status body &optional cause)
  "Signal a POLAR-HTTP-ERROR for OPERATION."
  (error 'polar-http-error
         :message   (format nil "Polar request failed: ~A" operation)
         :operation operation
         :status    status
         :body      body
         :cause     cause))


(defun polar--webhook-fail (message &optional cause)
  "Signal a POLAR-WEBHOOK-ERROR."
  (error 'polar-webhook-error
         :message   message
         :operation ':verify-webhook
         :cause     cause))


(defun polar--parse-fail (operation message &optional cause)
  "Signal a POLAR-PARSE-ERROR for OPERATION."
  (error 'polar-parse-error
         :message   message
         :operation operation
         :cause     cause))
