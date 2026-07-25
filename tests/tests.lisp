(in-package #:lispolar/tests)

;;;; -- Tiny test runner --

(defparameter *failures* nil)


(defun test-fail (name detail)
  (push (list name detail) *failures*)
  (format t "FAIL ~A: ~A~%" name detail))


(defun test-pass (name)
  (format t "ok   ~A~%" name))


(defmacro check (name form)
  `(handler-case
       (if ,form
           (test-pass ,name)
           (test-fail ,name (format nil "assertion failed: ~S" ',form)))
     (error (condition)
       (test-fail ,name condition))))


;;;; -- Cases --

(defun test-api-base-url ()
  (check "production default"
         (string= (api-base-url) "https://api.polar.sh"))
  (check "sandbox server"
         (string= (api-base-url :server "sandbox")
                  "https://sandbox-api.polar.sh"))
  (check "explicit base url wins"
         (string= (api-base-url :server "sandbox"
                                :base-url "https://example.test/api/")
                  "https://example.test/api")))


(defun test-checkout-link-helpers ()
  (check "uuid detection"
         (checkout-product-looks-like-uuid-p
          "2c61d091-c075-4c01-8c13-7077c8977985"))
  (check "non-uuid detection"
         (not (checkout-product-looks-like-uuid-p "polar_cl_demo")))
  (check "slug from bare id"
         (string= (checkout-link-slug "demo") "polar_cl_demo"))
  (check "slug from polar_cl prefix"
         (string= (checkout-link-slug "polar_cl_demo") "polar_cl_demo"))
  (check "slug from full url"
         (string= (checkout-link-slug "https://buy.polar.sh/polar_cl_demo?x=1")
                  "polar_cl_demo"))
  (check "checkout link url"
         (string= (checkout-link-url "demo")
                  "https://buy.polar.sh/polar_cl_demo"))
  (check "customer external id"
         (string= (customer-external-id 42 :prefix "hiisi-user")
                  "hiisi-user-42")))


(defun test-get-checkout-url ()
  (let ((url (get-checkout-url "polar_cl_demo"
                               "user@example.com"
                               7
                               :selected-plan "monthly"
                               :checkout-origin "https://app.example"
                               :external-id-prefix "hiisi-user")))
    (check "checkout url host"
           (and (search "https://buy.polar.sh/polar_cl_demo?" url)
                (search "customer_email=user%40example.com" url)
                (search "customer_external_id=hiisi-user-7" url)
                (or (search "metadata%5Bselected_plan%5D=monthly" url)
                    (search "metadata[selected_plan]=monthly" url))
                (search "success_url=" url)
                (search "return_url=" url)))))


(defun test-payload-parsing ()
  (let* ((checkout-json
           "{\"type\":\"checkout.updated\",\"data\":{\"id\":\"chk_1\",\"status\":\"succeeded\",\"customer_email\":\"a@b.c\",\"customer_external_id\":\"user-1\",\"product_id\":\"prod_1\",\"metadata\":{\"user_id\":1}}}")
         (subscription-json
           "{\"type\":\"subscription.active\",\"data\":{\"id\":\"sub_1\",\"status\":\"active\",\"current_period_end\":\"2026-08-01T00:00:00Z\",\"product\":{\"id\":\"prod_2\",\"name\":\"Monthly\"}}}")
         (checkout-payload (parse-webhook-payload checkout-json))
         (subscription-payload (parse-webhook-payload subscription-json))
         (checkout (webhook-payload-as-checkout checkout-payload))
         (subscription (webhook-payload-as-subscription subscription-payload)))
    (check "event type checkout"
           (eq (webhook-payload-event-type checkout-payload) :checkout.updated))
    (check "event type subscription"
           (eq (webhook-payload-event-type subscription-payload)
               :subscription.active))
    (check "checkout id"
           (string= (getf checkout :id) "chk_1"))
    (check "checkout status"
           (eq (getf checkout :status) :succeeded))
    (check "checkout product"
           (string= (getf checkout :product-id) "prod_1"))
    (check "subscription id"
           (string= (getf subscription :id) "sub_1"))
    (check "subscription status"
           (eq (getf subscription :status) :active))
    (check "subscription product nested"
           (string= (getf subscription :product-id) "prod_2"))
    (check "parse event type helper"
           (eq (parse-event-type "subscription.canceled")
               :subscription.canceled))
    (check "parse timestamp trim"
           (string= (parse-timestamp " 2026-08-01T00:00:00Z ")
                    "2026-08-01T00:00:00Z"))
    (check "parse-checkout-data direct"
           (string= (getf (parse-checkout-data
                            (lispolar::polar--json-get
                             (yason:parse checkout-json) "data"))
                          :customer-email)
                    "a@b.c"))
    (check "parse-subscription-data direct"
           (string= (getf (parse-subscription-data
                            (lispolar::polar--json-get
                             (yason:parse subscription-json) "data"))
                          :current-period-end)
                    "2026-08-01T00:00:00Z"))))


(defun test-webhook-signature ()
  (let* ((secret-bytes (lispolar::polar--string-bytes "test_secret"))
         (secret-b64 (lispolar::polar--base64-encode secret-bytes))
         (body "{\"type\":\"checkout.updated\",\"data\":{\"id\":\"chk_sig\",\"status\":\"open\"}}")
         (webhook-id "msg_123")
         (unix-now (- (get-universal-time)
                      (encode-universal-time 0 0 0 1 1 1970 0)))
         (timestamp (princ-to-string unix-now))
         (prefix (lispolar::polar--string-bytes
                  (format nil "~A.~A." webhook-id timestamp)))
         (body-bytes (lispolar::polar--string-bytes body))
         (signed (make-array (+ (length prefix) (length body-bytes))
                             :element-type '(unsigned-byte 8))))
    (replace signed prefix)
    (replace signed body-bytes :start1 (length prefix))
    (let* ((digest (lispolar::polar--hmac-sha256 secret-bytes signed))
           (signature (format nil "v1,~A"
                              (lispolar::polar--base64-encode digest)))
           (verified (verify-and-parse-webhook body webhook-id timestamp
                                               signature secret-b64
                                               :now (get-universal-time))))
      (check "valid signature accepts"
             (verify-webhook-signature body webhook-id timestamp
                                       signature secret-b64
                                       :now (get-universal-time)))
      (check "verified webhook id"
             (string= (lispolar:verified-webhook-id verified) webhook-id))
      (check "verified payload event"
             (eq (webhook-payload-event-type
                  (lispolar:verified-webhook-payload verified))
                 :checkout.updated))
      (check "invalid signature rejects"
             (handler-case
                 (progn
                   (verify-webhook-signature body webhook-id timestamp
                                             "v1,AAAA"
                                             secret-b64
                                             :now (get-universal-time))
                   nil)
               (polar-webhook-error ()
                 t)))
      (check "raw secret also works"
             (verify-webhook-signature body webhook-id timestamp
                                       signature "test_secret"
                                       :now (get-universal-time))))))


(defun run-tests ()
  "Run the lispolar unit tests. Return T on success, signal on failure."
  (setf *failures* nil)
  (test-api-base-url)
  (test-checkout-link-helpers)
  (test-get-checkout-url)
  (test-payload-parsing)
  (test-webhook-signature)
  (if *failures*
      (error "lispolar tests failed:~%~{~{  ~A: ~A~}~%~}"
             (reverse *failures*))
      (progn
        (format t "~%All lispolar tests passed.~%")
        t)))
