(in-package #:lispolar)

;;;; -- Customer portal sessions --

(defun create-customer-session (client external-customer-id
                                &key return-url)
  "Create a Polar customer session for EXTERNAL-CUSTOMER-ID.

Return a property list with :CUSTOMER-PORTAL-URL and optional :ID."
  (let* ((external-id
           (or (polar--trim external-customer-id)
               (polar--fail ':create-customer-session
                            "EXTERNAL-CUSTOMER-ID must be a non-empty string")))
         (origin (client-checkout-origin client))
         (body (polar--hash
                "external_customer_id" external-id
                "return_url" (or return-url
                                 (polar--checkout-return-url
                                  origin :path "/subscribe/manage"))))
         (response (polar--request client :post "/v1/customer-sessions"
                                   :body body))
         (portal-url
           (polar--trim
            (polar--json-string
             (polar--json-get response "customer_portal_url"))))
         (id (polar--trim (polar--json-string (polar--json-get response "id")))))
    (unless portal-url
      (polar--parse-fail
       ':create-customer-session
       "Polar customer session response did not contain a customer portal URL"))
    (append (list :customer-portal-url portal-url)
            (when id (list :id id))
            (list :raw response))))


(defun create-customer-portal-url (client user-id
                                   &key (external-id-prefix "user")
                                        return-url
                                        legacy-numeric-fallback)
  "Return a customer portal URL for USER-ID.

By default the external id is PREFIX-USER-ID. When LEGACY-NUMERIC-FALLBACK is
true and the namespaced id fails, retry with the bare numeric USER-ID string."
  (let ((external-id (customer-external-id user-id :prefix external-id-prefix)))
    (handler-case
        (getf (create-customer-session client external-id
                                       :return-url return-url)
              :customer-portal-url)
      (polar-http-error (condition)
        (if legacy-numeric-fallback
            (getf (create-customer-session client
                                           (princ-to-string user-id)
                                           :return-url return-url)
                  :customer-portal-url)
            (error condition))))))
