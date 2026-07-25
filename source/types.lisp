(in-package #:lispolar)

;;;; -- Event and status symbols --

(defparameter *event-type-names*
  '(("checkout.created"       . :checkout.created)
    ("checkout.updated"       . :checkout.updated)
    ("subscription.created"   . :subscription.created)
    ("subscription.updated"   . :subscription.updated)
    ("subscription.active"    . :subscription.active)
    ("subscription.canceled"  . :subscription.canceled)
    ("subscription.revoked"   . :subscription.revoked))
  "Known Polar webhook event type names.")

(defparameter *checkout-status-names*
  '(("open"      . :open)
    ("expired"   . :expired)
    ("confirmed" . :confirmed)
    ("succeeded" . :succeeded))
  "Known Polar checkout status names.")

(defparameter *subscription-status-names*
  '(("incomplete"         . :incomplete)
    ("incomplete_expired" . :incomplete-expired)
    ("trialing"           . :trialing)
    ("active"             . :active)
    ("past_due"           . :past-due)
    ("canceled"           . :canceled)
    ("unpaid"             . :unpaid))
  "Known Polar subscription status names.")


(defun polar--lookup-name (table value &optional default)
  "Look up VALUE in an alist of name to keyword, or return DEFAULT."
  (let ((string (polar--json-string value)))
    (if string
        (or (cdr (assoc string table :test #'string-equal))
            default
            :other)
        (or default :other))))


(defun parse-event-type (value)
  "Parse VALUE into a webhook event-type keyword."
  (polar--lookup-name *event-type-names* value :other))


(defun checkout-status (value)
  "Parse VALUE into a checkout status keyword."
  (polar--lookup-name *checkout-status-names* value :other))


(defun subscription-status (value)
  "Parse VALUE into a subscription status keyword."
  (polar--lookup-name *subscription-status-names* value :other))


(defun parse-timestamp (value)
  "Return a trimmed Polar timestamp string.

Returns NIL when VALUE is empty. Full calendar conversion is left to the
caller so lispolar stays free of date-library policy."
  (polar--trim (polar--json-string value)))


;;;; -- Payload structures --

(defstruct (webhook-payload (:constructor make-webhook-payload)
                            (:conc-name webhook-payload-))
  "Top-level Polar webhook payload."
  (event-type :other :type keyword)
  (data nil))


(defun polar--object-string (object &rest keys)
  "Return the first non-empty string value for KEYS in OBJECT."
  (dolist (key keys)
    (let ((value (polar--trim (polar--json-string (polar--json-get object key)))))
      (when value
        (return value)))))


(defun polar--object-plist (object &rest fields)
  "Build a property list from OBJECT using (KEY JSON-KEY...) FIELD specs."
  (loop for field in fields
        for key = (first field)
        for json-keys = (rest field)
        for value = (apply #'polar--object-string object json-keys)
        when value
          collect key and collect value))


(defun polar--parse-customer (object)
  "Parse nested Polar customer data into a property list."
  (when (polar--json-object-p object)
    (polar--object-plist object
                         '(:id "id")
                         '(:email "email")
                         '(:name "name"))))


(defun polar--parse-product (object)
  "Parse nested Polar product data into a property list."
  (when (polar--json-object-p object)
    (polar--object-plist object
                         '(:id "id")
                         '(:name "name"))))


(defun polar--parse-price (object)
  "Parse nested Polar price data into a property list."
  (when (polar--json-object-p object)
    (let ((amount (polar--json-get object "price_amount"))
          (currency (polar--object-string object "price_currency"))
          (interval (polar--object-string object "recurring_interval"))
          (id (polar--object-string object "id")))
      (append (when id (list :id id))
              (when amount (list :price-amount amount))
              (when currency (list :price-currency currency))
              (when interval (list :recurring-interval interval))))))


(defun polar--parse-metadata (object)
  "Return OBJECT when it is a hash-table metadata map."
  (when (polar--json-object-p object)
    object))


(defun parse-checkout-data (data)
  "Parse Polar checkout DATA into a property list."
  (unless (polar--json-object-p data)
    (polar--parse-fail ':parse-checkout-data "Checkout data must be a JSON object"))
  (let* ((status-value (polar--json-get data "status"))
         (customer (polar--parse-customer (polar--json-get data "customer")))
         (product (polar--parse-product (polar--json-get data "product")))
         (price (polar--parse-price (or (polar--json-get data "product_price")
                                        (polar--json-get data "price"))))
         (metadata (polar--parse-metadata (polar--json-get data "metadata")))
         (customer-metadata (polar--parse-metadata
                             (polar--json-get data "customer_metadata"))))
    (append
     (list :id (or (polar--object-string data "id")
                   (polar--parse-fail ':parse-checkout-data
                                      "Checkout data is missing id"))
           :status (checkout-status status-value))
     (let ((email (polar--object-string data "customer_email")))
       (when email (list :customer-email email)))
     (let ((external (or (polar--object-string data "customer_external_id")
                         (polar--object-string data "external_customer_id"))))
       (when external (list :customer-external-id external)))
     (let ((product-id (or (polar--object-string data "product_id" "productId")
                           (getf product :id))))
       (when product-id (list :product-id product-id)))
     (when customer (list :customer customer))
     (when product (list :product product))
     (when price (list :product-price price))
     (when metadata (list :metadata metadata))
     (when customer-metadata (list :customer-metadata customer-metadata)))))


(defun parse-subscription-data (data)
  "Parse Polar subscription DATA into a property list."
  (unless (polar--json-object-p data)
    (polar--parse-fail ':parse-subscription-data
                       "Subscription data must be a JSON object"))
  (let* ((status-value (polar--json-get data "status"))
         (customer (polar--parse-customer (polar--json-get data "customer")))
         (product (polar--parse-product (polar--json-get data "product")))
         (price (polar--parse-price (polar--json-get data "price")))
         (metadata (polar--parse-metadata (polar--json-get data "metadata")))
         (customer-metadata (polar--parse-metadata
                             (polar--json-get data "customer_metadata"))))
    (append
     (list :id (or (polar--object-string data "id")
                   (polar--parse-fail ':parse-subscription-data
                                      "Subscription data is missing id"))
           :status (subscription-status status-value))
     (let ((start (polar--object-string data "current_period_start")))
       (when start (list :current-period-start start)))
     (let ((end (polar--object-string data "current_period_end")))
       (when end (list :current-period-end end)))
     (let ((external (or (polar--object-string data "customer_external_id")
                         (polar--object-string data "external_customer_id"))))
       (when external (list :customer-external-id external)))
     (let ((product-id (or (polar--object-string data "product_id" "productId")
                           (getf product :id))))
       (when product-id (list :product-id product-id)))
     (when customer (list :customer customer))
     (when product (list :product product))
     (when price (list :price price))
     (when metadata (list :metadata metadata))
     (when customer-metadata (list :customer-metadata customer-metadata)))))


(defun parse-webhook-payload (body)
  "Parse raw webhook BODY bytes or string into a WEBHOOK-PAYLOAD."
  (let* ((text (if (stringp body)
                   body
                   (polar--bytes-string body)))
         (json
           (handler-case
               (yason:parse text)
             (error (condition)
               (polar--parse-fail ':parse-webhook-payload
                                  "Webhook body is not valid JSON"
                                  condition)))))
    (unless (polar--json-object-p json)
      (polar--parse-fail ':parse-webhook-payload
                         "Webhook payload must be a JSON object"))
    (make-webhook-payload
     :event-type (parse-event-type (polar--json-get json "type"))
     :data (polar--json-get json "data"))))


(defun webhook-payload-as-checkout (payload)
  "Return checkout data from PAYLOAD when it is a checkout event."
  (check-type payload webhook-payload)
  (case (webhook-payload-event-type payload)
    ((:checkout.created :checkout.updated)
     (parse-checkout-data (webhook-payload-data payload)))
    (otherwise nil)))


(defun webhook-payload-as-subscription (payload)
  "Return subscription data from PAYLOAD when it is a subscription event."
  (check-type payload webhook-payload)
  (case (webhook-payload-event-type payload)
    ((:subscription.created
      :subscription.updated
      :subscription.active
      :subscription.canceled
      :subscription.revoked)
     (parse-subscription-data (webhook-payload-data payload)))
    (otherwise nil)))
