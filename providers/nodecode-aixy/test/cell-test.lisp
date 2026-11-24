;;;; cell-test.lisp --- the aixy cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store here is a temp auth.json named through :auth-path, and every
;;;; wire a stubbed NLE:HTTP-FETCH or DEX:POST: nothing touches the network,
;;;; the environment or the operator's files. The gateway is the plugin's own
;;;; default, and a base the section names is any other.

(in-package #:nodecode.test)

(define-test-slice "aixy" "AIXY-CELL-" :start nodecode-aixy:start-cell)

(define-cell-lifecycle-tests "aixy"
  (:hooks 'nle::models-catalog-table 'nle::walk-provider-stream)
  (:command "aixy")
  (:refused ("base_url" 5)))

;;; --- fixtures ----------------------------------------------------------------

(defparameter +listing+
  (concatenate 'string
               "{\"object\":\"list\",\"data\":["
               "{\"id\":\"aixy-large\",\"context_length\":200000},"
               "{\"id\":\"aixy-fast\"},"
               "{\"id\":\"aixy-large\",\"context_length\":1},"
               "{\"id\":\"\"}]}")
  "A gateway catalog in the OpenAI listing's shape: three rows, one id twice, one nameless.")

(defmacro with-aixy-catalog ((&optional (body '+listing+)) &body forms)
  "Run FORMS with every NLE:HTTP-FETCH answering BODY, or failing as a refused
dial when BODY is :unreachable."
  `(with-stubbed-fdefinition
       (nle:http-fetch (url &rest args)
        (declare (ignore url args))
        (if (eq ,body :unreachable)
            (values nil "connection refused" t)
            ,body))
     ,@forms))

(defun aixy-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-aixy-round ((url headers body &optional config) model &body forms)
  "FORMS with the cell started (its section CONFIG) and one chat round for aixy
MODEL captured: URL, HEADERS and BODY (the decoded request) as dex:post saw
them."
  `(with-cell-stop ((apply #'aixy-start ,config))
     (let ((nle::*provider* "aixy") (nle::*model* ,model) (nle::*api-key* "sk-test")
           (nle::*endpoint* nil) (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (make-truncated-sse-stream
                     "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                     "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                     "[DONE]")
                    200))
         (nle::call-provider-streaming (user-context)))
       ,@forms)))

;;; --- the row, the address ----------------------------------------------------

(deftest aixy-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((aixy-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "aixy")))
      (is (equal "Aixy" (nlk:json-value row :string "name")))
      (is (equal "https://api.aixy-gateway.com/v1" (nlk:json-value row :string "api")))
      (is (equal '("AIXY_API_KEY") (coerce (nlk:json-value row :array "env") 'list)) "the key is the derived variable")
      (is (equal "openai-completions" (nle::configured-provider-lane "aixy")) "the chat lane drives it")
      (is (equal "https://api.aixy-gateway.com/v1/chat/completions"
                 (nle::lane-endpoint "aixy" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "aixy")) "and the row goes at stop")))

(deftest aixy-cell-base-follows-the-section ()
  (with-cell-stop ((aixy-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "aixy" "api")))
    (is (equal "https://relay.example/v1/chat/completions"
               (nle::lane-endpoint "aixy" "openai-completions")))))

(deftest aixy-cell-refuses-a-base-a-key-should-not-go-to ()
  ;; an empty member reads as absent, so the default base stands; the rest are refused
  (with-cell-stop ((aixy-start "base_url" ""))
    (is (equal "https://api.aixy-gateway.com/v1"
               (nlk:json-value (nle::models-catalog-table) :string "aixy" "api"))))
  (dolist (bad '("api.aixy-gateway.com/v1" "http://api.aixy-gateway.com/v1"
                 "https://user:pw@api.example/v1" "https://api.example/v1?key=1"
                 "https://api.example/v1#x" "ftp://api.example/v1" "https://api.example:0/v1"))
    (let ((text (refusal-text nlk:config-refusal (aixy-start "base_url" bad))))
      (is (search "base_url" text) (format nil "~s is refused, naming the member" bad))))
  ;; a gateway on this machine answers over plain http
  (with-cell-stop ((aixy-start "base_url" "http://127.0.0.1:8080/v1"))
    (is (equal "http://127.0.0.1:8080/v1"
               (nlk:json-value (nle::models-catalog-table) :string "aixy" "api")))))

;;; --- the gateway's own catalog ------------------------------------------------

(deftest aixy-cell-listing-asks-the-gateway-with-the-key ()
  (with-cell-stop ((aixy-start))
    (with-temp-auth (auth "{\"api_keys\":{\"aixy\":{\"provider\":\"aixy\",\"key\":\"sk-aixy\"}}}")
      (let ((asked nil) (sent nil))
        (with-stubbed-fdefinition
            (nle:http-fetch (url &rest args)
             (setf asked url sent (getf args :headers))
             (values +listing+ 200))
          (nlk:bind ((nle::*auth-file-path* auth))
            (multiple-value-bind (rows error) (nle::list-provider-models "aixy")
              (is (null error))
              (is (equal "https://api.aixy-gateway.com/v1/models" asked) "the listing is the gateway's")
              (is (equal "Bearer sk-aixy" (aixy-header sent "authorization")) "with the project key on it")
              (is (equal '("aixy-fast" "aixy-large")
                         (mapcar (lambda (row) (getf row :id)) rows)) "and its ids are the picker's, sorted and each once")
              (is (equal 200000 (getf (find "aixy-large" rows :key (lambda (row) (getf row :id))
                                                            :test #'string=)
                                       :context-window)) "with the window the gateway advertises"))))))))

(deftest aixy-cell-command-lists-what-the-gateway-serves ()
  (with-cell-stop ((aixy-start))
    (with-temp-auth (auth "{}")
      (nlk:bind ((nle::*auth-file-path* auth))
        (with-aixy-catalog ()
          (let ((text (cell-entry "nodecode-aixy" "aixy" "")))
            (is (search "Aixy at https://api.aixy-gateway.com/v1 serves 2 models" text) "one id twice is listed once")
            (is (search "aixy-large (200,000 context tokens)" text) "and its advertised window")
            (is (search "  aixy-fast" text) "with an id that advertises none")
            (is (search "no key yet" text) "and where a key comes from")))
        (with-aixy-catalog (:unreachable)
          (let ((text (cell-entry "nodecode-aixy" "aixy" "models")))
            (is (search "connection refused" text) "an unreachable gateway says so")
            (is (search "AIXY_API_KEY" text) "and still names the key to set"))))
      (with-aixy-catalog ()
        (let ((text (cell-entry "nodecode-aixy" "aixy" "key")))
          (is (search "Make a project key at https://aixy-gateway.com" text))
          (is (search "connect" text)))))))

;;; --- the turn -----------------------------------------------------------------

(deftest aixy-cell-turn-carries-the-session-header ()
  (with-stubbed-fdefinition (nle:turn () (list :session-id "conv-42"))
    (with-aixy-round (url headers body) "aixy-large"
      (is (equal "https://api.aixy-gateway.com/v1/chat/completions" url))
      (is (equal "conv-42" (aixy-header headers "x-aixy-session-id")) "the conversation is named to Aixy")
      (is (equal "aixy-large" (gethash "model" body)) "and the turn asks for the id the gateway serves"))))

(deftest aixy-cell-session-header-follows-the-section ()
  (with-stubbed-fdefinition (nle:turn () (list :session-id "conv-42"))
    (with-aixy-round (url headers body '("session_header" "X-My-Scope")) "aixy-large"
      (is (equal "conv-42" (aixy-header headers "x-my-scope")) "the section names the header"))
    (with-aixy-round (url headers body '("session_header" "none")) "aixy-large"
      (is (null (aixy-header headers "x-aixy-session-id")) "\"none\" sends none"))))
