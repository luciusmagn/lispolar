(in-package #:lispolar)

;;;; -- HTTP client --

(defun polar--endpoint (client path)
  "Return the absolute URL for PATH on CLIENT."
  (format nil "~A~A"
          (string-right-trim '(#\/) (client-api-base-url client))
          (if (and (plusp (length path)) (char= (char path 0) #\/))
              path
              (concatenate 'string "/" path))))


(defun polar--json-encode (value)
  "Encode VALUE as a JSON string with YASON."
  (with-output-to-string (stream)
    (yason:encode value stream)))


(defun polar--hash (&rest keys-and-values)
  "Build a string-keyed hash-table from KEYS-AND-VALUES."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash (if (stringp key)
                                key
                                (string-downcase (symbol-name key)))
                            table)
                   value))
    table))


(defun polar--request (client method path &key body)
  "Perform an authenticated JSON METHOD request to PATH on CLIENT.

Return a YASON value. Signal POLAR-HTTP-ERROR on non-success responses."
  (let* ((url (polar--endpoint client path))
         (headers `(("Authorization" . ,(format nil "Bearer ~A"
                                                (client-access-token client)))
                    ("Accept" . "application/json")
                    ("User-Agent" . ,(client-user-agent client))
                    ,@(when body
                        '(("Content-Type" . "application/json")))))
         (payload (when body (polar--json-encode body))))
    (handler-case
        (multiple-value-bind (response status)
            (dexador:request url
                             :method method
                             :headers headers
                             :content payload
                             :want-stream nil
                             :force-binary nil)
          (let* ((text (if (stringp response)
                           response
                           (polar--bytes-string response)))
                 (parsed
                   (if (and text (plusp (length (string-trim '(#\Space #\Tab #\Newline) text))))
                       (handler-case
                           (yason:parse text)
                         (error (condition)
                           (polar--parse-fail method
                                              (format nil "Failed to parse Polar response from ~A"
                                                      path)
                                              condition)))
                       nil)))
            (unless (<= 200 status 299)
              (polar--http-fail method status (or text "")))
            parsed))
      (http-request-failed (condition)
        (let* ((status (dexador:response-status condition))
               (body (ignore-errors
                      (let ((raw (dexador:response-body condition)))
                        (if (stringp raw)
                            raw
                            (polar--bytes-string raw))))))
          (polar--http-fail method
                            (or status 0)
                            (or body (princ-to-string condition))
                            condition)))
      (polar-error (condition)
        (error condition))
      (error (condition)
        (polar--fail method
                     (format nil "Polar request to ~A failed" path)
                     condition)))))
