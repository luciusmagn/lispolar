(asdf:defsystem #:lispolar
  :description "Common Lisp client for the Polar.sh API and webhooks"
  :author "Lukáš Hozda"
  :license "ISC"
  :version "0.1.0"
  :serial t
  :depends-on (#:cl-base64
               #:dexador
               #:ironclad
               #:quri
               #:yason)
  :components ((:module "source"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "config")
                             (:file "encoding")
                             (:file "types")
                             (:file "client")
                             (:file "checkouts")
                             (:file "customer-sessions")
                             (:file "webhooks"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:lispolar/tests))))

(asdf:defsystem #:lispolar/tests
  :description "Tests for lispolar"
  :depends-on (#:lispolar)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:lispolar/tests '#:run-tests)))
