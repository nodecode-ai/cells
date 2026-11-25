;;;; Explicit installed-Pi integration slice: synthetic stores, no provider calls.
;;;; This is deliberately not loaded by the portable native cell test system.
(in-package #:nodecode.test)

(define-test-slice "openai-codex-pi-integration" "OPENAI-CODEX-PI-INTEGRATION-")

(deftest openai-codex-pi-integration-private-pipe-with-synthetic-store ()
  (with-temp-auth (pi-store "{\"openai-codex-2\":{\"type\":\"oauth\",\"access\":\"synthetic-access\",\"refresh\":\"synthetic-refresh\",\"expires\":4102444800000,\"accountId\":\"synthetic-account\"}}")
    (let* ((entry (nlk:json-object "pi_auth_file" (namestring pi-store)))
           (answer (nodecode-openai-codex::call-pi-helper "openai-codex-2" entry nil))
           (probe (nodecode-openai-codex::call-pi-helper "openai-codex-2" entry t)))
      (is (equal "synthetic-access" (nlk:json-value answer :text "key")))
      (is (equal "synthetic-account" (nlk:json-value answer :text "accountId")))
      (is (null (nlk:json-value probe :text "key")))
      (is (null (nlk:json-value probe :text "accountId"))))))
