(in-package #:lispolar)

;;;; -- Checkout helpers --

(defun customer-external-id (user-id &key (prefix "user"))
  "Return a stable Polar external customer id for USER-ID.

PREFIX defaults to \"user\", producing values such as \"user-42\"."
  (format nil "~A-~A" prefix user-id))


(defun checkout-product-looks-like-uuid-p (product-id)
  "Return true when PRODUCT-ID looks like a UUID product identifier."
  (let ((value (polar--trim product-id)))
    (when value
      (and (= (length value) 36)
           (char= (char value 8) #\-)
           (char= (char value 13) #\-)
           (char= (char value 18) #\-)
           (char= (char value 23) #\-)
           (every (lambda (char)
                    (or (digit-char-p char 16)
                        (char= char #\-)))
                  value)))))


(defun checkout-link-slug (product-id)
  "Normalize PRODUCT-ID into a Polar checkout-link slug.

Accepts UUIDs, bare slugs, polar_cl_* values, and full checkout URLs."
  (let* ((trimmed (polar--trim product-id))
         (without-quotes (when trimmed
                           (string-trim '(#\") trimmed)))
         (without-query (when without-quotes
                          (subseq without-quotes
                                  0
                                  (or (position #\? without-quotes)
                                      (length without-quotes)))))
         (without-slash (when without-query
                          (string-right-trim '(#\/) without-query)))
         (extracted
           (cond
             ((null without-slash) nil)
             ((or (and (>= (length without-slash) 8)
                       (string-equal "https://" without-slash :end2 8))
                  (and (>= (length without-slash) 7)
                       (string-equal "http://" without-slash :end2 7)))
              (let* ((parts (uiop:split-string without-slash :separator "/"))
                     (last (car (last (remove-if (lambda (part)
                                                   (zerop (length part)))
                                                 parts)))))
                (or last without-slash)))
             (t without-slash))))
    (unless extracted
      (polar--fail ':checkout-link-slug "PRODUCT-ID must be a non-empty string"))
    (if (and (>= (length extracted) 9)
             (string= "polar_cl_" extracted :end2 9))
        extracted
        (concatenate 'string "polar_cl_" extracted))))


(defun checkout-link-url (product-id)
  "Return the buy.polar.sh URL for PRODUCT-ID."
  (format nil "https://buy.polar.sh/~A" (checkout-link-slug product-id)))


(defun polar--checkout-success-url (origin &key selected-plan)
  "Build a success URL under ORIGIN, optionally including SELECTED-PLAN."
  (let ((base (format nil "~A/subscribe/success"
                      (string-right-trim '(#\/) origin))))
    (if selected-plan
        (format nil "~A?plan=~A" base (polar--url-encode selected-plan))
        base)))


(defun polar--checkout-return-url (origin &key (path "/subscribe/plans"))
  "Build a return URL under ORIGIN."
  (format nil "~A~A"
          (string-right-trim '(#\/) origin)
          (if (and (plusp (length path)) (char= (char path 0) #\/))
              path
              (concatenate 'string "/" path))))


(defun get-checkout-url (product-id user-email user-id
                         &key selected-plan
                              (checkout-origin *default-checkout-origin*)
                              (external-id-prefix "user")
                              success-url
                              return-url)
  "Build a Polar checkout-link URL with customer metadata query parameters.

This path does not call the Polar API. It is useful when PRODUCT-ID is a
checkout-link slug rather than a UUID product id."
  (let* ((email (or (polar--trim user-email)
                    (polar--fail ':get-checkout-url
                                 "USER-EMAIL must be a non-empty string")))
         (origin (or (polar--trim checkout-origin)
                     *default-checkout-origin*))
         (external-id (customer-external-id user-id :prefix external-id-prefix))
         (user-id-string (princ-to-string user-id))
         (success (or success-url
                      (polar--checkout-success-url origin
                                                   :selected-plan selected-plan)))
         (return (or return-url
                     (polar--checkout-return-url origin)))
         (pairs (list (cons "customer_email" email)
                      (cons "customer_external_id" external-id)
                      (cons "metadata[user_id]" user-id-string)
                      (cons "metadata[email]" email)
                      (cons "customer_metadata[user_id]" user-id-string)
                      (cons "customer_metadata[email]" email)
                      (cons "success_url" success)
                      (cons "return_url" return))))
    (when selected-plan
      (setf pairs
            (append pairs
                    (list (cons "metadata[selected_plan]" selected-plan)
                          (cons "customer_metadata[selected_plan]" selected-plan)))))
    (format nil "~A?~A"
            (checkout-link-url product-id)
            (format nil "~{~A~^&~}"
                    (mapcar (lambda (pair)
                              (format nil "~A=~A"
                                      (car pair)
                                      (polar--url-encode (cdr pair))))
                            pairs)))))


(defun create-checkout (client product-id user-email user-id
                        &key selected-plan
                             success-url
                             return-url
                             (external-id-prefix "user")
                             metadata
                             customer-metadata)
  "Create a Polar checkout session through the API.

Return a property list with at least :URL and :ID when present."
  (let* ((product (or (polar--trim product-id)
                      (polar--fail ':create-checkout
                                   "PRODUCT-ID must be a non-empty string")))
         (email (or (polar--trim user-email)
                    (polar--fail ':create-checkout
                                 "USER-EMAIL must be a non-empty string")))
         (origin (client-checkout-origin client))
         (external-id (customer-external-id user-id :prefix external-id-prefix))
         (meta (let ((table (if metadata
                                metadata
                                (polar--hash "user_id" user-id "email" email))))
                 (when (and selected-plan (polar--json-object-p table))
                   (setf (gethash "selected_plan" table) selected-plan))
                 table))
         (customer-meta
           (let ((table (if customer-metadata
                            customer-metadata
                            (polar--hash "user_id" user-id "email" email))))
             (when (and selected-plan (polar--json-object-p table))
               (setf (gethash "selected_plan" table) selected-plan))
             table))
         (body (polar--hash
                "products" (list product)
                "success_url" (or success-url
                                  (polar--checkout-success-url
                                   origin :selected-plan selected-plan))
                "return_url" (or return-url
                                 (polar--checkout-return-url origin))
                "customer_email" email
                "customer_external_id" external-id
                "metadata" meta
                "customer_metadata" customer-meta))
         (response (polar--request client :post "/v1/checkouts" :body body))
         (url (polar--trim (polar--json-string (polar--json-get response "url"))))
         (id (polar--trim (polar--json-string (polar--json-get response "id")))))
    (unless url
      (polar--parse-fail ':create-checkout
                         "Polar checkout response did not contain a checkout URL"))
    (append (list :url url)
            (when id (list :id id))
            (list :raw response))))


(defun create-checkout-url (client product-id user-email user-id
                            &key selected-plan
                                 success-url
                                 return-url
                                 (external-id-prefix "user")
                                 (allow-link-fallback t))
  "Return a checkout URL for PRODUCT-ID and USER-EMAIL.

When PRODUCT-ID is not a UUID and ALLOW-LINK-FALLBACK is true, build a
checkout-link URL without calling the API. Otherwise create an API session."
  (let ((product (or (polar--trim product-id)
                     (polar--fail ':create-checkout-url
                                  "PRODUCT-ID must be a non-empty string"))))
    (if (and allow-link-fallback
             (not (checkout-product-looks-like-uuid-p product)))
        (get-checkout-url product user-email user-id
                          :selected-plan selected-plan
                          :checkout-origin (client-checkout-origin client)
                          :external-id-prefix external-id-prefix
                          :success-url success-url
                          :return-url return-url)
        (getf (create-checkout client product user-email user-id
                               :selected-plan selected-plan
                               :success-url success-url
                               :return-url return-url
                               :external-id-prefix external-id-prefix)
              :url))))
