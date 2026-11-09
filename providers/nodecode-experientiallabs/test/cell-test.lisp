;;;; cell-test.lisp --- the experientiallabs cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through the *AUTH-FILE-PATH* the
;;;; chain reads, every key variable a stubbed NLE::CREDENTIAL-ENV, every Cost
;;;; API read a stubbed NLK:HTTP: nothing touches the network, the environment
;;;; or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "experientiallabs" "EXPERIENTIALLABS-CELL-" :start nodecode-experientiallabs:start-cell)

(define-cell-lifecycle-tests "experientiallabs"
  (:hooks 'nle::models-catalog-table :credential)
  (:command "explabs")
  (:refused ("usage_timeout_seconds" 0)))

(defparameter +wallet-body+
  "{\"data\":{\"total_credits\":10,\"total_usage\":2.5}}"
  "A /credits answer: ten dollars bought, two fifty spent.")

(defparameter +cap-body+
  "{\"data\":{\"limit\":5,\"limit_remaining\":4}}"
  "A /key answer: a five dollar daily cap with four left.")

(defun explabs-ask (args)
  "Run /explabs with ARGS, as the slash catalog does."
  (cell-entry "nodecode-experientiallabs" "explabs" (or args "")))

(defmacro with-explabs-key ((&optional (key "sk-explabs")) &body body)
  "FORMS with a scratch auth.json holding nothing but an experientiallabs
KEY, and the credential chain reading it."
  `(with-temp-auth (auth (if ,key
                              (format nil "{\"api_keys\":{\"experientiallabs\":{\"provider\":\"experientiallabs\",\"key\":\"~a\"}}}"
                                      ,key)
                              "{}"))
     (let ((nle::*auth-file-path* auth)) ,@body)))

(defmacro with-cost-api ((credits key) &body forms)
  "FORMS with every NLK:HTTP call answered by CREDITS and KEY, each a body or
NIL for a refused read."
  `(with-stubbed-fdefinition
       (nlk:http (method url &rest args)
        (declare (ignore method args))
        (let ((body (if (search "/credits" url) ,credits ,key)))
          (if body (values body 200 nil) (values "{\"error\":\"nope\"}" 500 nil))))
     ,@forms))

(deftest experientiallabs-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((experientiallabs-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "experientiallabs")))
      (is (equal "Experiential Labs" (nlk:json-value row :string "name")))
      (is (equal "https://api.experientiallabs.ai/v1" (nlk:json-value row :string "api")))
      (is (gethash "qwen3.8-27b" (nlk:json-value row :object "models")) "the curated fallback is listed")
      (is (gethash "glm-5.3-flash" (nlk:json-value row :object "models")) "and the auxiliary model with it")
      (is (equal "openai-completions" (nle::configured-provider-lane "experientiallabs")) "the chat lane drives it")
      (is (equal "https://api.experientiallabs.ai/v1/chat/completions"
                 (nle::lane-endpoint "experientiallabs" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "experientiallabs")))))

(deftest experientiallabs-cell-base-follows-the-section ()
  (with-cell-stop ((experientiallabs-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1" (nlk:json-value (nle::models-catalog-table) :string "experientiallabs" "api")))))

(deftest experientiallabs-cell-reads-its-own-key-variables ()
  (with-cell-stop ((experientiallabs-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (cond ((equal name "EXPLABS_API_KEY") "sk-first")
                                       ((equal name "EXPERIENTIALLABS_API_KEY") "sk-second")))
        (let ((credential (nle::resolve-provider-credential "experientiallabs" :auth-path auth :probe t)))
          (is (equal "sk-first" (nle:credential-key credential)) "the primary variable answers first")
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "EXPERIENTIALLABS_API_KEY") "sk-second"))
        (is (equal "sk-second" (nle:credential-key (nle::resolve-provider-credential "experientiallabs" :auth-path auth :probe t))) "the long variable answers next")))
    (with-temp-auth (auth "{\"api_keys\":{\"experientiallabs\":{\"provider\":\"experientiallabs\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "EXPLABS_API_KEY") "sk-first"))
        (is (equal "sk-saved" (nle:credential-key (nle::resolve-provider-credential "experientiallabs" :auth-path auth :probe t))) "a saved key outranks the variable")))))

(deftest experientiallabs-cell-never-sends-another-familys-key ()
  (with-cell-stop ((experientiallabs-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "experientiallabs" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest experientiallabs-cell-answers-the-wallet-and-the-cap ()
  (with-cell-stop ((experientiallabs-start))
    (with-explabs-key ()
      (let ((asked nil) (sent-key nil))
        (with-stubbed-fdefinition
            (nlk:http (method url &rest args)
             (declare (ignore method))
             (push url asked)
             (setf sent-key (cdr (assoc "authorization" (getf args :headers) :test #'string-equal)))
             (values (if (search "/credits" url) +wallet-body+ +cap-body+) 200 nil))
          (let ((text (explabs-ask "")))
            (is (search "$7.50" text) "the wallet's remainder")
            (is (search "$1.00 of $5.00" text) "the day's spend against its cap")
            (is (equal "Bearer sk-explabs" sent-key) "the key rides as a bearer header")
            (is (equal 2 (length asked)) "both Cost API reads were made")))))))

(deftest experientiallabs-cell-says-what-to-do-without-a-key ()
  (with-cell-stop ((experientiallabs-start))
    (with-explabs-key (nil)
      (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
        (with-cost-api (+wallet-body+ +cap-body+)
          (is (search "no key saved" (explabs-ask "")) "it names /connect and the variable"))))))

(deftest experientiallabs-cell-fails-open-on-a-dead-cost-api ()
  (with-cell-stop ((experientiallabs-start))
    (with-explabs-key ()
      (with-cost-api (nil nil)
        (let ((text (explabs-ask "")))
          (is (search "did not answer" text) "a refused read is a sentence, not a signal")
          (is (not (search "wallet" text)) "and claims no balance it does not have"))))))
