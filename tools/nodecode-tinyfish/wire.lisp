;;;; wire.lisp --- the TinyFish API, one function per operation.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The same hosts and the same calls the original plugin makes:
;;;;
;;;;   GET  {search_url}?query=...                     TinyFish Search
;;;;   POST {fetch_url} {urls, format}                  TinyFish Fetch
;;;;   POST {agent_url}/v1/automation/run-async         a web agent run
;;;;   GET  {agent_url}/v1/runs/{run_id}                that run's state
;;;;   GET  {agent_url}/v1/profiles                     saved site sign-ins
;;;;   POST {agent_url}/v1/profiles                     create one
;;;;   GET  {agent_url}/v1/wallet                       credits left
;;;;
;;;; Search and Fetch are free; a run bills TinyFish credits. The key rides
;;;; the X-API-Key header, as in the original, and every call answers
;;;; (values VALUE STATUS). *HTTP* and *SLEEP* are the two seams a test
;;;; stubs, so no test dials anything and none waits on a clock.

(in-package #:nodecode-tinyfish)

(defparameter *http* #'nlk:http
  "The exchange this cell dials through: NLE's HTTP, stubbed by a test.")

(defparameter *sleep* #'sleep
  "How the agent's poll waits: SLEEP, stubbed out by a test.")

(defparameter +client-name+ "nodecode"
  "What the API records as the calling client, the way the plugin sent its own.")

(defparameter +key-env+ "TINYFISH_API_KEY"
  "The variable the original plugin also reads when its config carries no key.")

(defparameter +key-url+ "https://agent.tinyfish.ai/api-keys"
  "Where a free key is made.")

(defparameter +terminal-statuses+ '("COMPLETED" "FAILED" "CANCELLED")
  "The run states the agent stops waiting on.")

;;; --- who and where ------------------------------------------------------------

(defun env-secret (name)
  "The environment variable NAME's value, trimmed, or NIL."
  (let ((value (uiop:getenv name)))
    (and value (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun api-key ()
  "The key the section carries, else the key in the environment."
  (or (let ((key (nlk:trimmed (or (setting :api-key) ""))))
        (and (plusp (length key)) key))
      (env-secret +key-env+)))

(defun configured-p ()
  "Whether a key is there to ask with at all."
  (and (api-key) t))

(defun unaware ()
  "Refuse: nothing can be asked without a key."
  (unless (configured-p)
    (fail "no TinyFish API key: set tinyfish.api_key in /settings, or export ~a (free at ~a)"
          +key-env+ +key-url+))
  t)

(defun base (service)
  "SERVICE's base URL: the section's own, so a test or a proxy names another."
  (string-right-trim "/" (or (ecase service
                                 (:search (setting :search-url))
                                 (:fetch (setting :fetch-url))
                                 (:agent (setting :agent-url)))
                               "")))

(defun headers ()
  `(("x-api-key" . ,(api-key))
    ("accept" . "application/json")
    ("content-type" . "application/json")
    ("x-tf-client-name" . ,+client-name+)
    ("x-tf-client-version" . ,+version+)))

;;; --- the one exchange ---------------------------------------------------------

(defun sentence (body)
  "What a refused response said, one line, at most 300 characters, or NIL."
  (let ((text (typecase body
                (string body)
                (t (ignore-errors (nlk:encode-json-object body))))))
    (when (and text (plusp (length text)))
      (let ((flat (substitute #\Space #\Newline text)))
        (subseq flat 0 (min 300 (length flat)))))))

(defun refused (what status body)
  "Refuse: WHAT was answered STATUS, saying BODY where it said anything."
  (fail "~a answered HTTP ~d~@[: ~a~]" what status (sentence body)))

(defun exchange (method url &key object)
  "Send OBJECT as JSON to URL. => (values DECODED STATUS)."
  (unaware)
  (let ((timeout (setting :timeout-seconds)))
    (multiple-value-bind (body status)
        (funcall *http* method url
                 :headers (headers)
                 :content (and object (nlk:encode-json-object object))
                 :timeout timeout
                 :connect-timeout (min 10 timeout))
      (values (let ((value (and (stringp body) (ignore-errors (nlk:decode-json body)))))
                (or value body))
              status))))

(defun answered (what method url &key object)
  "EXCHANGE, refusing anything outside 2xx with WHAT named."
  (multiple-value-bind (value status) (exchange method url :object object)
    (if (<= 200 status 299)
        value
        (refused what status value))))

;;; --- search and fetch: the free half ------------------------------------------

(defun search-json (query)
  "TinyFish Search's answer for QUERY, decoded."
  (answered "TinyFish Search" "GET"
            (format nil "~a?query=~a" (base :search) (quri:url-encode query))))

(defun fetch-json (urls &key (format "markdown"))
  "TinyFish Fetch's answer for URLS, decoded."
  (answered "TinyFish Fetch" "POST" (base :fetch)
            :object (nlk:json-object "urls" (coerce urls 'vector)
                                      "format" format)))

;;; --- the agent: the half that spends credits ----------------------------------

(defun agent-call (what method path &key object)
  "PATH under the agent host, refused with WHAT named."
  (answered what method (concatenate 'string (base :agent) path) :object object))

(defun start-run (url goal &key browser-profile proxy-country profile-id)
  "Enqueue a run; answers immediately with its run_id."
  (let ((body (nlk:json-object "url" url
                                "goal" goal
                                "browser_profile" (or browser-profile "lite")
                                "api_integration" +client-name+)))
    (when (and proxy-country (plusp (length proxy-country)))
      (setf (gethash "proxy_config" body)
            (nlk:json-object "enabled" t "country_code" proxy-country)))
    (when (and profile-id (plusp (length profile-id)))
      (setf (gethash "use_profile" body) t
            (gethash "profile_id" body) profile-id))
    (agent-call "TinyFish Agent" "POST" "/v1/automation/run-async" :object body)))

(defun run-json (run-id)
  (agent-call "TinyFish Agent" "GET"
              (format nil "/v1/runs/~a" (quri:url-encode run-id))))

(defun run-status (run)
  "RUN's state, upcased, or the empty string."
  (string-upcase (or (nlk:json-value run :string "status") "")))

(defun await-run (run-id)
  "Poll RUN-ID until it is terminal or agent_timeout_seconds pass.
=> (values RUN FINISHED-P)."
  (let ((deadline (+ (get-universal-time) (setting :agent-timeout-seconds)))
        (interval (setting :poll-seconds))
        (run nil))
    (loop
      (setf run (run-json run-id))
      (when (member (run-status run) +terminal-statuses+ :test #'equal)
        (return (values run t)))
      (when (>= (get-universal-time) deadline)
        (return (values run nil)))
      (funcall *sleep* interval))))

;;; --- saved sign-ins and the wallet --------------------------------------------

(defun profiles-json ()
  (agent-call "TinyFish Profiles" "GET" "/v1/profiles"))

(defun create-profile-json (fields)
  (agent-call "TinyFish Profiles" "POST" "/v1/profiles" :object fields))

(defun setup-url (profile-id domain)
  "Where the operator signs in to a site on PROFILE-ID; nothing bills until
it is opened."
  (format nil "~a/profiles/~a/setup?~@[domain=~a&~]source=nodecode"
          (base :agent) (quri:url-encode profile-id)
          (and domain (plusp (length domain)) (quri:url-encode domain))))

(defun wallet-json ()
  "The account's balance and rates; NIL when the account has no wallet yet
(legacy billing), which the original also read as an answer, not a failure."
  (multiple-value-bind (value status) (exchange "GET" (format nil "~a/v1/wallet" (base :agent)))
    (cond ((= status 404) nil)
          ((<= 200 status 299) value)
          (t (refused "TinyFish Wallet" status value)))))

;;; --- what a tool with no arguments advertises ---------------------------------

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))
