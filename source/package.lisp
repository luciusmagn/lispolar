(defpackage #:lispolar
  (:use #:cl)
  (:import-from #:cl-base64
                #:base64-string-to-usb8-array
                #:usb8-array-to-base64-string)
  (:import-from #:dexador
                #:http-request-failed)
  (:import-from #:quri
                #:url-encode)
  (:export
   ;; conditions
   #:polar-error
   #:polar-error-message
   #:polar-error-operation
   #:polar-error-status
   #:polar-error-body
   #:polar-error-cause
   #:polar-http-error
   #:polar-webhook-error
   #:polar-parse-error
   ;; configuration
   #:*default-api-base-url*
   #:*sandbox-api-base-url*
   #:*default-checkout-origin*
   #:*webhook-timestamp-tolerance-seconds*
   #:api-base-url
   #:make-client
   #:client
   #:client-access-token
   #:client-api-base-url
   #:client-checkout-origin
   #:client-user-agent
   ;; checkouts
   #:checkout-link-slug
   #:checkout-link-url
   #:checkout-product-looks-like-uuid-p
   #:create-checkout
   #:create-checkout-url
   #:customer-external-id
   #:get-checkout-url
   ;; customer sessions
   #:create-customer-session
   #:create-customer-portal-url
   ;; types and payload helpers
   #:checkout-status
   #:event-type
   #:parse-checkout-data
   #:parse-event-type
   #:parse-subscription-data
   #:parse-timestamp
   #:parse-webhook-payload
   #:subscription-status
   #:webhook-payload
   #:webhook-payload-data
   #:webhook-payload-event-type
   #:webhook-payload-as-checkout
   #:webhook-payload-as-subscription
   ;; webhooks
   #:verify-webhook-signature
   #:verified-webhook
   #:verified-webhook-id
   #:verified-webhook-payload
   #:verify-and-parse-webhook))

(defpackage #:lispolar/tests
  (:use #:cl)
  (:import-from #:lispolar
                #:api-base-url
                #:checkout-link-slug
                #:checkout-link-url
                #:checkout-product-looks-like-uuid-p
                #:customer-external-id
                #:get-checkout-url
                #:parse-checkout-data
                #:parse-event-type
                #:parse-subscription-data
                #:parse-timestamp
                #:parse-webhook-payload
                #:verify-and-parse-webhook
                #:verify-webhook-signature
                #:webhook-payload-as-checkout
                #:webhook-payload-as-subscription
                #:webhook-payload-event-type)
  (:export #:run-tests))
