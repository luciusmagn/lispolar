(in-package #:lispolar)

;;;; -- Configuration --

(defparameter *default-api-base-url* "https://api.polar.sh"
  "Production Polar API origin.")

(defparameter *sandbox-api-base-url* "https://sandbox-api.polar.sh"
  "Sandbox Polar API origin.")

(defparameter *default-checkout-origin* "http://localhost:8888"
  "Default application origin used when building success and return URLs.")

(defparameter *webhook-timestamp-tolerance-seconds* (* 5 60)
  "Maximum absolute age of an accepted webhook timestamp, in seconds.")


(defun api-base-url (&key server base-url)
  "Return the Polar API origin for SERVER or BASE-URL.

BASE-URL wins when non-empty. SERVER may be \"sandbox\" (case-insensitive)
or any other value for production. Defaults to production."
  (let ((configured (and base-url (string-trim '(#\Space #\Tab #\Newline) base-url))))
    (cond
      ((and configured (plusp (length configured)))
       (string-right-trim '(#\/) configured))
      ((and server (string-equal (string-trim '(#\Space #\Tab #\Newline) server)
                                 "sandbox"))
       *sandbox-api-base-url*)
      (t
       *default-api-base-url*))))


(defclass client ()
  ((access-token
    :initarg  :access-token
    :reader   client-access-token
    :type     string
    :documentation "Bearer token used for authenticated Polar API calls.")
   (api-base-url
    :initarg  :api-base-url
    :reader   client-api-base-url
    :type     string
    :documentation "Polar API origin without a trailing slash.")
   (checkout-origin
    :initarg  :checkout-origin
    :reader   client-checkout-origin
    :type     string
    :documentation "Application origin used for success and return URLs.")
   (user-agent
    :initarg  :user-agent
    :reader   client-user-agent
    :type     string
    :documentation "User-Agent header sent with Polar requests."))
  (:documentation "Configured Polar API client."))


(defun make-client (&key access-token
                         server
                         base-url
                         (checkout-origin *default-checkout-origin*)
                         (user-agent "lispolar/0.1.0"))
  "Construct a Polar CLIENT.

ACCESS-TOKEN is required for API session creation and customer portal
sessions. SERVER and BASE-URL select the API origin."
  (let* ((token (and access-token
                     (string-trim '(#\Space #\Tab #\Newline) access-token)))
         (origin (string-right-trim
                  '(#\/)
                  (string-trim '(#\Space #\Tab #\Newline)
                               (or checkout-origin *default-checkout-origin*)))))
    (unless (and token (plusp (length token)))
      (polar--fail ':make-client "ACCESS-TOKEN must be a non-empty string"))
    (unless (and origin (plusp (length origin)))
      (polar--fail ':make-client "CHECKOUT-ORIGIN must be a non-empty string"))
    (make-instance 'client
                   :access-token    token
                   :api-base-url    (api-base-url :server server :base-url base-url)
                   :checkout-origin origin
                   :user-agent      (or user-agent "lispolar/0.1.0"))))
