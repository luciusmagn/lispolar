(in-package #:lispolar)

;;;; -- Webhook signature verification --

(defstruct (verified-webhook (:constructor make-verified-webhook)
                             (:conc-name verified-webhook-))
  "A webhook payload accepted after signature verification."
  (id nil :type string)
  (payload nil :type webhook-payload))


(defun polar--parse-signature-values (signature-header)
  "Extract signature material from a Standard Webhooks signature header."
  (let* ((parts (remove-if (lambda (part) (zerop (length part)))
                           (mapcar (lambda (part)
                                     (string-trim '(#\Space #\Tab) part))
                                   (uiop:split-string signature-header
                                                      :separator ","))))
         (signatures '())
         (index 0)
         (count (length parts)))
    (loop while (< index count)
          do (let ((part (nth index parts)))
               (cond
                 ((and (plusp (length part)) (char= (char part 0) #\v))
                  (let ((eq-pos (position #\= part)))
                    (cond
                      (eq-pos
                       (let ((value (subseq part (1+ eq-pos))))
                         (when (plusp (length value))
                           (push value signatures))))
                      ((< (1+ index) count)
                       (push (nth (1+ index) parts) signatures)
                       (incf index)))))
                 (t
                  (push part signatures))))
             (incf index))
    (nreverse signatures)))


(defun polar--decode-signature-bytes (encoded)
  "Decode a signature token as standard or URL-safe Base64."
  (or (polar--base64-decode encoded)
      (polar--base64url-decode encoded)))


(defun polar--secret-candidates (secret-value)
  "Return candidate signing keys for SECRET-VALUE.

Polar secrets may arrive as raw text, standard Base64, Base64URL, or with a
polar_whs_ prefix."
  (let* ((trimmed (or (polar--trim secret-value)
                      (polar--webhook-fail "Webhook secret is empty")))
         (candidates '())
         (add (lambda (bytes)
                (when (and bytes (plusp (length bytes)))
                  (unless (member bytes candidates :test #'equalp)
                    (push bytes candidates))))))
    (funcall add (polar--base64-decode trimmed))
    (funcall add (polar--base64url-decode trimmed))
    (when (and (>= (length trimmed) 10)
               (string= "polar_whs_" trimmed :end2 10))
      (funcall add (polar--base64url-decode (subseq trimmed 10)))
      (funcall add (polar--base64-decode (subseq trimmed 10))))
    (funcall add (polar--string-bytes trimmed))
    (nreverse candidates)))


(defun polar--hmac-sha256 (key message)
  "Return the HMAC-SHA256 digest of MESSAGE under KEY."
  (let ((mac (ironclad:make-mac :hmac key :sha256)))
    (ironclad:update-mac mac message)
    (ironclad:produce-mac mac)))


(defun polar--signatures-match-p (computed provided-list)
  "Return true when COMPUTED equals any signature in PROVIDED-LIST."
  (some (lambda (provided)
          (and (= (length computed) (length provided))
               (every #'= computed provided)))
        provided-list))


(defun verify-webhook-signature (body webhook-id webhook-timestamp
                                 signature-header secret-value
                                 &key (tolerance-seconds
                                       *webhook-timestamp-tolerance-seconds*)
                                      (now (get-universal-time)))
  "Verify a Polar webhook using the Standard Webhooks signed-message format.

BODY may be a string or octet vector. Signals POLAR-WEBHOOK-ERROR on failure.
Returns T on success."
  (let* ((id (or (polar--trim webhook-id)
                 (polar--webhook-fail "Missing webhook-id")))
         (timestamp (or (polar--trim webhook-timestamp)
                        (polar--webhook-fail "Missing webhook-timestamp")))
         (header (or (polar--trim signature-header)
                     (polar--webhook-fail "Missing webhook-signature")))
         (body-bytes (if (stringp body)
                         (polar--string-bytes body)
                         body))
         (signatures (polar--parse-signature-values header)))
    (unless signatures
      (polar--webhook-fail "Invalid signature format: could not extract signature"))
    (let ((provided (remove nil (mapcar #'polar--decode-signature-bytes signatures))))
      (unless provided
        (polar--webhook-fail "Invalid signature format: not valid base64"))
      (let* ((prefix (polar--string-bytes (format nil "~A.~A." id timestamp)))
             (signed (make-array (+ (length prefix) (length body-bytes))
                                 :element-type '(unsigned-byte 8)))
             (secrets (polar--secret-candidates secret-value))
             (valid nil))
        (replace signed prefix)
        (replace signed body-bytes :start1 (length prefix))
        (dolist (secret secrets)
          (when (polar--signatures-match-p (polar--hmac-sha256 secret signed)
                                           provided)
            (setf valid t)
            (return)))
        (unless valid
          (polar--webhook-fail "Webhook signature verification failed"))
        (let ((timestamp-value
                (handler-case
                    (parse-integer timestamp)
                  (error (condition)
                    (polar--webhook-fail "Invalid timestamp format" condition))))
              ;; Standard Webhooks timestamps are Unix seconds.
              (unix-now (- now (encode-universal-time 0 0 0 1 1 1970 0))))
          (when (> (abs (- timestamp-value unix-now)) tolerance-seconds)
            (polar--webhook-fail "Webhook timestamp outside tolerance window"))
          t)))))


(defun verify-and-parse-webhook (body webhook-id webhook-timestamp
                                 signature-header secret-value
                                 &key (tolerance-seconds
                                       *webhook-timestamp-tolerance-seconds*)
                                      (now (get-universal-time)))
  "Verify BODY and return a VERIFIED-WEBHOOK with the parsed payload."
  (verify-webhook-signature body webhook-id webhook-timestamp
                            signature-header secret-value
                            :tolerance-seconds tolerance-seconds
                            :now now)
  (make-verified-webhook
   :id (polar--trim webhook-id)
   :payload (parse-webhook-payload body)))
