;;;; cell-test.lisp --- the home-assistant cell against a stubbed instance.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Home Assistant endpoint is a stubbed NLK:HTTP and the token a
;;;; stubbed NLE::CREDENTIAL-ENV: nothing reaches an instance, the environment
;;;; or the operator's files. Each tool is called the way a model's call
;;;; reaches it — through the real dispatch, contract and all — so the schema
;;;; the provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "home-assistant" "HOME-ASSISTANT-CELL-")

(defun home-assistant-config (&rest pairs)
  "A whole shared config holding the `homeassistant' section PAIRS make, with
its url and token variable defaulted."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "url" section "http://ha.test:8123")
    (alexandria:ensure-gethash "token_env" section "HASS_TOKEN")
    (nlk:json-object "homeassistant" section)))

(defun home-assistant-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-home-assistant:start-cell (apply #'home-assistant-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-ha-token (&body body)
  "BODY with the token environment answering one fixed token."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "HASS_TOKEN") "test-token"))
     ,@body))

(defmacro with-ha-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT), oldest first,
once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Home Assistant request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +states+
  (concatenate
   'string
   "[{\"entity_id\":\"light.kitchen\",\"state\":\"on\","
   "\"attributes\":{\"friendly_name\":\"Kitchen Light\",\"brightness\":180},"
   "\"last_changed\":\"2026-01-01T10:00:00+00:00\",\"last_updated\":\"2026-01-01T10:00:05+00:00\"},"
   "{\"entity_id\":\"switch.kettle\",\"state\":\"off\","
   "\"attributes\":{\"friendly_name\":\"Kettle\"},"
   "\"last_changed\":\"2026-01-01T09:00:00+00:00\",\"last_updated\":\"2026-01-01T09:00:00+00:00\"}]")
  "Two entities, as GET /api/states answers them.")

(defparameter +kitchen+
  (concatenate
   'string
   "{\"entity_id\":\"light.kitchen\",\"state\":\"on\","
   "\"attributes\":{\"friendly_name\":\"Kitchen Light\",\"brightness\":180},"
   "\"last_changed\":\"2026-01-01T10:00:00+00:00\",\"last_updated\":\"2026-01-01T10:00:05+00:00\"}")
  "One entity, as GET /api/states/light.kitchen answers it.")

(defparameter +services+
  (concatenate
   'string
   "[{\"domain\":\"light\",\"services\":{\"turn_on\":{\"description\":\"Turn the light on\","
   "\"fields\":{\"brightness\":{\"description\":\"0 to 255\"}}}}},"
   "{\"domain\":\"switch\",\"services\":{\"turn_on\":{\"description\":\"Switch it on\"}}}]")
  "What GET /api/services answers.")

(defun ha-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun ha-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (ha-answer id tool arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "home-assistant"
  (:config "token_env" "HASS_TOKEN")
  (:help :home-assistant)
  (:fixture with-ha-token)
  (:refused ("url" 5) ("token_env" 5)))

(deftest home-assistant-cell-without-a-token-refuses ()
  ;; The one setting no verb can work without: the section names a token, and
  ;; a section that names none is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (home-assistant-start "token_env" ""))))
    (is (search "no token" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "ha_list_entities")) "and installs nothing")))

;;; --- ha_list_entities ---------------------------------------------------------------

(deftest home-assistant-cell-lists-entities (with-ha-token)
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked ("/api/states" (values +states+ 200)))
      (let* ((object (ha-object "ha-list-1" "ha_list_entities" "{\"domain\":\"light\"}"))
             (entities (nlk:json-array object "entities")))
        (is (= 1 (nlk:json-value object :integer "count")) "the domain filter kept one entity")
        (is (equal "light.kitchen" (nlk:json-value (aref entities 0) :string "entity_id")))
        (is (equal "Kitchen Light" (nlk:json-value (aref entities 0) :string "friendly_name")))
        (is (= 1 (length asked)) "a plain list is one REST call")
        (is (equal :get (first (first asked))))))))

(deftest home-assistant-cell-caps-a-list-and-says-so (with-ha-token)
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked ("/api/states" (values +states+ 200)))
      (let ((object (ha-object "ha-list-2" "ha_list_entities" "{\"max\":1}")))
        (is (= 1 (nlk:json-value object :integer "count")) "the per-call max wins")
        (is (eq t (nlk:json-value object :boolean "truncated")))
        (is (search "of 2 matching entities"
                    (nlk:json-value object :string "truncated_note"))
            "and the note says how many there were")))))

(deftest home-assistant-cell-filters-by-area-and-name (with-ha-token)
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked ("/api/states" (values +states+ 200)))
      (is (= 1 (length (nlk:json-array
                         (ha-object "ha-list-3" "ha_list_entities" "{\"area\":\"kitchen\"}")
                         "entities")))
          "an area matches the room word in a friendly name")
      (is (= 1 (length (nlk:json-array
                         (ha-object "ha-list-4" "ha_list_entities" "{\"name\":\"kettle\"}")
                         "entities")))
          "a name matches a friendly name or an entity id"))))

;;; --- ha_get_state --------------------------------------------------------------------

(deftest home-assistant-cell-reads-one-state (with-ha-token)
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked ("/api/states/light.kitchen" (values +kitchen+ 200)))
      (let ((object (ha-object "ha-state-1" "ha_get_state" "{\"entity_id\":\"light.kitchen\"}")))
        (is (equal "on" (nlk:json-value object :string "state")))
        (is (= 180 (nlk:json-value object :integer "attributes" "brightness"))
            "every attribute comes through")
        (is (equal "2026-01-01T10:00:00+00:00" (nlk:json-value object :string "last_changed"))
            "the timestamp is Home Assistant's own"))
      (is (equal "http://ha.test:8123/api/states/light.kitchen" (second (first asked)))
          "the entity id rides the path"))))

(deftest home-assistant-cell-refuses-a-state-it-may-not-read (with-ha-token)
  (with-cell-stop ((home-assistant-start "entity_denylist" (vector "light.*")))
    (with-ha-wire (asked)
      (let ((answer (ha-answer "ha-state-2" "ha_get_state" "{\"entity_id\":\"light.kitchen\"}")))
        (is (search "entity_denylist" answer) "the refusal names the list that blocked it")
        (is (null asked) "and it goes out before any request")))))

(deftest home-assistant-cell-refuses-a-malformed-entity-id (with-ha-token)
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked)
      (let ((answer (ha-answer "ha-state-3" "ha_get_state" "{\"entity_id\":\"Light Kitchen\"}")))
        (is (search "invalid entity_id format" answer))
        (is (null asked) "nothing was asked of the instance")))))

(deftest home-assistant-cell-refuses-a-call-with-no-entity-id (with-ha-token)
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked)
      (let ((answer (ha-answer "ha-state-4" "ha_get_state" "{}")))
        (is (search "preflight" answer) "the contract refuses it before the body")
        (is (search "entity_id" answer))
        (is (null asked))))))

;;; --- ha_list_services ----------------------------------------------------------------

(deftest home-assistant-cell-lists-services (with-ha-token)
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked ("/api/services" (values +services+ 200)))
      (let ((object (ha-object "ha-svc-1" "ha_list_services" "{\"domain\":\"light\"}")))
        (is (= 1 (nlk:json-value object :integer "count")) "the domain filter kept one domain")
        (is (search "Turn the light on" (nlk:encode-json-object object))
            "the service and its field descriptions come through")))))

;;; --- ha_call_service -------------------------------------------------------------------

(deftest home-assistant-cell-calls-a-service (with-ha-token)
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked ("/api/services/light/turn_on"
                          (values "[{\"entity_id\":\"light.kitchen\",\"state\":\"on\"}]" 200)))
      (let ((object (ha-object "ha-call-1" "ha_call_service"
                                "{\"domain\":\"light\",\"service\":\"turn_on\",\"entity_id\":\"light.kitchen\",\"data\":{\"brightness\":255}}")))
        (is (eq t (nlk:json-value object :boolean "success")))
        (is (equal "light.turn_on" (nlk:json-value object :string "service")))
        (is (equal "light.kitchen"
                   (nlk:json-value (aref (nlk:json-array object "affected_entities") 0)
                                   :string "entity_id"))
            "the answer names the entity that changed"))
      (let ((payload (nlk:decode-json (third (first asked)))))
        (is (equal :post (first (first asked))))
        (is (equal "light.kitchen" (nlk:json-value payload :string "entity_id"))
            "entity_id rides the payload Home Assistant reads")
        (is (= 255 (nlk:json-value payload :integer "brightness"))
            "and the caller's own data with it")))))

(deftest home-assistant-cell-refuses-a-code-running-service-domain (with-ha-token)
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked)
      (let ((answer (ha-answer "ha-call-2" "ha_call_service"
                                "{\"domain\":\"shell_command\",\"service\":\"restart_caddy\"}")))
        (is (search "blocked for security" answer) "shell_command never reaches the instance")
        (is (search "rest_command" answer) "and the refusal lists the whole blocklist")
        (is (null asked))))))

(deftest home-assistant-cell-explains-a-call-that-changed-nothing (with-ha-token)
  ;; Home Assistant answers an empty array when nothing changed: the entity may
  ;; be missing, in another domain, or already in the asked state. The cell
  ;; reads it and says which, rather than reporting a success nobody saw.
  (with-cell-stop ((home-assistant-start))
    (with-ha-wire (asked ("/api/states/light.kitchen" (values +kitchen+ 200))
                         ("/api/services/switch/turn_on" (values "[]" 200)))
      (let ((object (ha-object "ha-call-3" "ha_call_service"
                                "{\"domain\":\"switch\",\"service\":\"turn_on\",\"entity_id\":\"light.kitchen\"}")))
        (is (eq t (nlk:json-value object :boolean "nothing_changed")))
        (is (search "in the 'light' domain" (nlk:json-value object :string "error"))
            "the answer names the domain that would work")
        (is (= 2 (length asked)) "the service, then the read that explains it")))))
