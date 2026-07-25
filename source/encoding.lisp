(in-package #:lispolar)

;;;; -- Encoding helpers --

(defun polar--string-bytes (string)
  "Return UTF-8 octets for STRING."
  #+sbcl
  (sb-ext:string-to-octets string :external-format :utf-8)
  #-sbcl
  (map '(simple-array (unsigned-byte 8) (*))
       #'char-code
       string))


(defun polar--bytes-string (bytes)
  "Return a UTF-8 string for BYTES."
  #+sbcl
  (sb-ext:octets-to-string bytes :external-format :utf-8)
  #-sbcl
  (map 'string #'code-char bytes))


(defun polar--base64-encode (bytes)
  "Encode BYTES as standard Base64 without line breaks."
  (usb8-array-to-base64-string bytes :columns nil))


(defun polar--base64-decode (string)
  "Decode standard Base64 STRING into octets, or NIL on failure."
  (handler-case
      (base64-string-to-usb8-array string)
    (error ()
      nil)))


(defun polar--base64url-decode (string)
  "Decode Base64URL STRING without padding into octets, or NIL on failure."
  (let* ((normalized (substitute #\+ #\- (substitute #\/ #\_ string)))
         (remainder (mod (length normalized) 4))
         (padded (ecase remainder
                   (0 normalized)
                   (2 (concatenate 'string normalized "=="))
                   (3 (concatenate 'string normalized "="))
                   (1 nil))))
    (when padded
      (polar--base64-decode padded))))


(defun polar--url-encode (value)
  "Percent-encode VALUE for a query component."
  (url-encode (if (stringp value)
                  value
                  (princ-to-string value))
              :encoding :utf-8))


(defun polar--trim (value)
  "Return VALUE trimmed of surrounding whitespace, or NIL when empty."
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
      (when (plusp (length trimmed))
        trimmed))))


(defun polar--json-object-p (value)
  "Return true when VALUE is a YASON hash-table object."
  (hash-table-p value))


(defun polar--json-get (object key)
  "Return KEY from a YASON hash-table OBJECT, or NIL."
  (when (polar--json-object-p object)
    (or (gethash key object)
        (gethash (string-downcase key) object))))


(defun polar--json-string (value)
  "Coerce VALUE to a string when possible."
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((symbolp value) (string-downcase (symbol-name value)))
    (t (princ-to-string value))))


(defun polar--json-keyword (value)
  "Coerce VALUE to a keyword when possible."
  (let ((string (polar--json-string value)))
    (when string
      (intern (string-upcase string) :keyword))))
