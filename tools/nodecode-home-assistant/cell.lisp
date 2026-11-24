;;;; cell.lisp --- Home Assistant's four tools, over the instance's REST API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :home-assistant) answers the primer below, and every request's help
;;;; section carries one line naming it. Nothing hooks the provider path: an
;;;; instance speaks HTTP, and NLK:HTTP is the one seam.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "homeassistant": {"url": "http://homeassistant.local:8123",
;;;;                     "token_env": "HASS_TOKEN"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from NousResearch's Hermes Agent plugin (see NOTICE): the four
;;;; tools and their schemas, the filters each one takes, the operator
;;;; allow/deny lists and what they refuse on a service call, the blocklist of
;;;; code-running service domains, and the shapes the tools answer with.

(in-package #:nodecode-home-assistant)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Home Assistant is available through the nodecode-home-assistant cell: four tools, which
a person can also call from eval in the home-assistant package.
  ha_list_entities  {domain, area, name, entity_ids, max}
      The entities of the instance, with their friendly names. Use the filters: a bare call
      on a large install answers thousands. For a room, filter by area (Home Assistant's own
      room registry is not read here, so an area matches the room word in an entity's friendly
      name, its area attribute or its id); for a device you cannot name, filter by name or
      domain. An explicit entity_ids list supersedes the others. A result cut by a cap is
      marked truncated with a note on how to narrow it.
  ha_get_state  {entity_id}
      One entity's state and every attribute (brightness, setpoint, a sensor reading), with
      last_changed and last_updated as Home Assistant reports them, in UTC.
  ha_list_services  {domain}
      The services (actions) the instance offers, per domain, each with a description and the
      fields it accepts. Use it to learn how to drive a device found by ha_list_entities.
  ha_call_service  {domain, service, entity_id, data}
      Call one service. The service domain must match the entity's own domain: a light.*
      entity needs light.turn_on, a switch.* entity switch.turn_on; the wrong domain affects
      nothing. data is a JSON object of the service's own fields, e.g. {\"brightness\": 255}
      for light.turn_on. Never guess an entity id: call ha_list_entities first and use an id
      it answered. An answer says which entities changed; when none did it says why — a
      missing entity, another domain, or already in the asked state.
The domains shell_command, command_line, python_script, pyscript, hassio and rest_command are
refused: they run code on the Home Assistant host or reach the local network. When the operator
set entity_allowlist or entity_denylist, only permitted entities may be listed, read or
controlled, and a call that names no entity, an area, a device, a floor or a label is refused
unless it is notify.*, persistent_notification.* or a script."
  "What (help :home-assistant) answers while the cell runs.")

;;; --- what the instance constrains ----------------------------------------------

(defparameter +blocked-domains+
  '("command_line" "hassio" "python_script" "pyscript" "rest_command" "shell_command")
  "Service domains that run arbitrary code on the Home Assistant host or reach
its local network. Home Assistant has no service-level access control, so this
list is the only guard a service call has.")

(defparameter +non-entity-domains+ '("notify" "persistent_notification")
  "Domains whose services deliver a message and act on no entity, so a call
naming no entity is allowed there even under the operator lists.")

(defparameter +script-builtin-services+ '("reload" "toggle" "turn_off" "turn_on")
  "Script services that address no script entity; any other script.<name> runs
the entity script.<name>, and is checked as that entity.")

(defparameter +indirect-target-keys+ '("area_id" "device_id" "floor_id" "label_id")
  "Target keys Home Assistant resolves on the server. The operator lists
cannot check them, so a filtered call that uses one is refused.")

(defparameter +name-pattern+ "\\A[a-z][a-z0-9_]*\\z"
  "A domain or service name: lowercase, digits, underscore, and nothing else.")

(defparameter +entity-id-pattern+ "\\A[a-z_][a-z0-9_]*\\.[a-z0-9_]+\\z"
  "A Home Assistant entity id, e.g. light.kitchen: no LF, no path.")

(defparameter +note-chars+ 300
  "How much of Home Assistant's own error body a refusal quotes back.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (home-assistant-error (condition)
       (nle:failure (format nil "ERROR: ~a" (home-assistant-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Home Assistant: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer-detail (body)
  "Home Assistant's own words for a failed call, clipped, or NIL."
  (let ((text (nlk:trimmed (or body ""))))
    (and (plusp (length text)) (nlk:clip text +note-chars+))))

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the bearer token the section names: read once, here."
  (declare (ignore table))
  (let ((url (string-right-trim "/" (or (getf values :url) "")))
        (env (nonblank (getf values :token-env)))
        (file (nonblank (getf values :token-file)))
        (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of token_env and token_file, not both"))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "url ~s has no http:// or https:// scheme: set it to the full base URL of your instance, e.g. \"https://ha.example.com\" or \"http://homeassistant.local:8123\"" url))
    (setf (getf plist :url) url
          (getf plist :token)
          (cond (env (or (nonblank (nle::credential-env env))
                         (nlk:config-error "token_env names ~a, which is unset or empty" env)))
                (file (or (nonblank (ignore-errors
                                     (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "token_file ~a cannot be read, or is empty" file)))
                (t (nlk:config-error "no token: set token_env to the environment variable holding a long-lived access token, or token_file to a file holding one"))))
    plist))

;;; --- one REST exchange ----------------------------------------------------------

(defun exchange (method path &key payload)
  "One Home Assistant REST exchange => (values BODY STATUS), BODY the text the
instance answered, STATUS its status code. A dial that fails is a refusal."
  (let ((url (concatenate 'string (setting :url) path)))
    (handler-case
        (multiple-value-bind (body status)
            (nlk:http method url
                      :headers (list (cons "authorization"
                                           (format nil "Bearer ~a" (setting :token)))
                                     (cons "accept" "application/json")
                                     (cons "content-type" "application/json"))
                      :content (and payload (nlk:encode-json-object payload))
                      :timeout (setting :timeout)
                      :connect-timeout (setting :timeout))
          (values (if (stringp body) body (nlk:body-text body)) status))
      (error (condition)
        (fail "~a ~a did not answer: ~a" method url
              (nlk:one-line (princ-to-string condition)))))))

(defun api-call (method path &key payload)
  "PATH's decoded JSON answer, or a refusal carrying Home Assistant's own words."
  (multiple-value-bind (body status) (exchange method path :payload payload)
    (if (and (integerp status) (< status 400))
        (or (ignore-errors (nlk:decode-json body))
            (and (plusp (length (nlk:trimmed (or body "")))) body))
        (fail "~a ~a answered HTTP ~a~@[: ~a~]" method path status (answer-detail body)))))

;;; --- the operator's entity lists ------------------------------------------------

(defun glob-regex (pattern)
  "PATTERN as an anchored regular expression: * is any run, ? one character."
  (with-output-to-string (out)
    (write-string "\\A" out)
    (loop for character across pattern
          do (case character
               (#\* (write-string ".*" out))
               (#\? (write-char #\. out))
               (t (when (find character ".+()[]{}^$|\\") (write-char #\\ out))
                  (write-char character out))))
    (write-string "\\z" out)))

(defun entity-matches-p (entity-id pattern)
  "Whether ENTITY-ID matches PATTERN: exact, a glob (* and ?), or a family
prefix, where `.` and `_` are interchangeable at the boundary so
office_thermostat_* covers office_thermostat.0_valve_position. A bare pattern
with no trailing separator is exact-or-glob only, so climate.office does not
swallow climate.office_zombie."
  (and (stringp pattern)
       (plusp (length pattern))
       (or (string= entity-id pattern)
           (and (ignore-errors (ppcre:scan (glob-regex pattern) entity-id)) t)
           (let ((last (char pattern (1- (length pattern)))))
             (when (find last "*._")
               (let ((base (string-right-trim "*._" pattern)))
                 (and (plusp (length base))
                      (uiop:string-prefix-p base entity-id)
                      (or (= (length entity-id) (length base))
                          (find (char entity-id (length base)) "._")))))))))

(defun matched-pattern (entity-id patterns)
  "The first pattern of PATTERNS that ENTITY-ID matches, or NIL."
  (find-if (lambda (pattern) (entity-matches-p entity-id pattern)) patterns))

(defun operator-lists ()
  "The section's allowlist and denylist, as two lists of patterns."
  (values (setting :entity-allowlist) (setting :entity-denylist)))

(defun permitted-p (entity-id)
  "NIL when the operator's lists let ENTITY-ID be read and controlled, else
why not: the denylist is a final veto, and an allowlist limits everything."
  (multiple-value-bind (allow deny) (operator-lists)
    (let ((hit (matched-pattern entity-id deny)))
      (cond (hit (format nil "~a is excluded by entity_denylist (pattern ~s)" entity-id hit))
            ((and allow (not (matched-pattern entity-id allow)))
             (format nil "~a is not in entity_allowlist" entity-id))
            (t nil)))))

;;; --- a service call's targets ----------------------------------------------------

(defun entity-key-p (key)
  "Whether a payload KEY names entities: entity_id, entities, snapshot_entities
or any *_entity_id(s) member."
  (or (member key '("entity_id" "entities" "snapshot_entities") :test #'string=)
      (uiop:string-suffix-p key "_entity_id")
      (uiop:string-suffix-p key "_entity_ids")))

(defun present-value-p (value)
  "Whether VALUE is a target worth checking: not NIL, not empty."
  (and value
       (not (and (stringp value) (zerop (length value))))
       (not (and (vectorp value) (zerop (length value))))
       (not (and (hash-table-p value) (zerop (hash-table-count value))))))

(defun id-list (value)
  "VALUE — an id, an array of ids, a comma- or space-separated string, or an
object whose keys are ids — as a list of lowercased entity ids."
  (let ((ids '()))
    (labels ((add (text)
               (when (stringp text)
                 (let ((text (string-downcase (nlk:trimmed text))))
                   (when (plusp (length text)) (pushnew text ids :test #'string=))))))
      (cond ((stringp value) (dolist (part (ppcre:split "[,\\s]+" value)) (add part)))
            ((vectorp value) (loop for item across value do (add item)))
            ((hash-table-p value) (loop for key being the hash-keys of value do (add key)))
            (t nil))
      ids)))

(defun service-targets (payload)
  "=> (values ENTITY-IDS INDIRECT-KEYS) the payload names, recursing into
nested objects; INDIRECT-KEYS are the area/device/floor/label targets a list
of patterns cannot check."
  (let ((ids '()) (indirect '()))
    (labels ((walk (object)
               (when (hash-table-p object)
                 (loop for key being the hash-keys of object using (hash-value value)
                       when (and (stringp key) (present-value-p value))
                         do (cond ((member key +indirect-target-keys+ :test #'string=)
                                   (pushnew key indirect :test #'string=))
                                  ((entity-key-p key)
                                   (setf ids (union ids (id-list value) :test #'string=)))
                                  ((hash-table-p value) (walk value)))))))
      (walk payload))
    (values ids indirect)))

(defun service-call-violation (domain service payload)
  "Why the operator's lists refuse DOMAIN.SERVICE with PAYLOAD, or NIL."
  (multiple-value-bind (allow deny) (operator-lists)
    (when (or allow deny)
      (let ((hint (concatenate 'string
                               "Call ha_call_service with explicit entity_id values the lists permit, "
                               "or ask the operator to adjust entity_allowlist / entity_denylist.")))
        (multiple-value-bind (ids indirect) (service-targets payload)
          (when indirect
            (return-from service-call-violation
              (format nil "~a.~a targets by ~{~a~^, ~}, which cannot be checked against the operator's entity lists. ~a"
                      domain service indirect hint)))
          (when (member "all" ids :test #'string=)
            (return-from service-call-violation
              (format nil "~a.~a with entity_id 'all' would act on every entity, which the operator's entity lists do not permit. ~a"
                      domain service hint)))
          (setf ids (remove "none" ids :test #'string=))
          (when (and (string= domain "script")
                     (not (member service +script-builtin-services+ :test #'string=)))
            (pushnew (format nil "script.~a" service) ids :test #'string=))
          (when (null ids)
            (unless (member domain +non-entity-domains+ :test #'string=)
              (return-from service-call-violation
                (format nil "~a.~a names no entity, so it could act on every ~a entity while the operator's entity lists are set. ~a"
                        domain service domain hint))))
          (dolist (entity-id (sort (copy-list ids) #'string<))
            (let ((hit (matched-pattern entity-id deny)))
              (when hit
                (return-from service-call-violation
                  (format nil "~a is excluded by entity_denylist (pattern ~s); ~a.~a refused. ~a"
                          entity-id hit domain service hint))))
            (when (and allow (not (matched-pattern entity-id allow)))
              (return-from service-call-violation
                (format nil "~a is not in entity_allowlist; ~a.~a refused. ~a"
                        entity-id domain service hint))))
          nil)))))

;;; --- ha_list_entities ------------------------------------------------------------

(defun state-id (state)
  (or (nlk:json-value state :string "entity_id") ""))

(defun state-name (state)
  (or (nlk:json-value state :string "attributes" "friendly_name") ""))

(defun operator-kept (states)
  "STATES the operator's lists allow: the denylist excludes, and an allowlist
restricts, before any other filter spends a token on them."
  (multiple-value-bind (allow deny) (operator-lists)
    (if (or allow deny)
        (remove-if-not (lambda (state)
                         (let ((id (state-id state)))
                           (and (not (matched-pattern id deny))
                                (or (null allow) (matched-pattern id allow)))))
                       states)
        states)))

(defun narrowed (states domain area name)
  "STATES kept by the caller's DOMAIN, AREA and NAME filters."
  (let ((states (if (nonblank domain)
                    (remove-if-not (lambda (state)
                                     (uiop:string-prefix-p (concatenate 'string domain ".")
                                                            (state-id state)))
                                   states)
                    states)))
    (when (nonblank area)
      (let ((area (string-downcase (nlk:trimmed area))))
        (setf states
              (remove-if-not
               (lambda (state)
                 (let ((attributes (nlk:json-value state :object "attributes")))
                   (or (search area (string-downcase
                                     (or (nlk:json-value attributes :string "friendly_name") "")))
                       (search area (string-downcase
                                     (or (nlk:json-value attributes :string "area") ""))))))
               states))))
    (when (nonblank name)
      (let ((name (string-downcase (nlk:trimmed name))))
        (setf states
              (remove-if-not
               (lambda (state)
                 (or (search name (string-downcase (state-name state)))
                     (search name (string-downcase (state-id state)))))
               states))))
    states))

(defun list-entities (call)
  "The ha_list_entities tool: the instance's entities, filtered and capped."
  (answering
    (let* ((states (coerce (nlk:json-array (api-call :get "/api/states")) 'list))
           (ids (id-list (coerce (or (entity-list-entity-ids call) #()) 'list)))
           (asked (entity-list-max-entities call))
           (cap (or (and (integerp asked) (plusp asked) asked)
                    (setting :max-entities)))
           (matched (if ids
                        (remove-if-not (lambda (state)
                                         (member (state-id state) ids :test #'string=))
                                       (operator-kept states))
                        (narrowed (operator-kept states)
                                  (entity-list-domain call)
                                  (entity-list-area call)
                                  (entity-list-name call))))
           (total (length matched))
           (shown (if (and cap (> total cap)) (subseq matched 0 cap) matched))
           (answer (nlk:json-object
                    "count" (length shown)
                    "entities" (map 'vector
                                    (lambda (state)
                                      (nlk:json-object "entity_id" (state-id state)
                                                       "state" (or (nlk:json-value state :string "state") "")
                                                       "friendly_name" (state-name state)))
                                    shown))))
      (when (and cap (> total cap))
        (setf (gethash "truncated" answer) t
              (gethash "truncated_note" answer)
              (format nil "Showing first ~a of ~a matching entities. Refine with domain/area/name filters or an explicit entity_ids list, or pass a larger max."
                      cap total)))
      (nlk:encode-json-object answer))))

;;; --- ha_get_state ----------------------------------------------------------------

(defun read-state (call)
  "The ha_get_state tool: one entity's state and every attribute."
  (answering
    (let ((entity-id (nlk:trimmed (or (state-read-entity-id call) ""))))
      (unless (ppcre:scan +entity-id-pattern+ entity-id)
        (return-from read-state (refuse "invalid entity_id format: ~s" entity-id)))
      (let ((excluded (permitted-p entity-id)))
        (when excluded
          (return-from read-state
            (refuse "~a. Use ha_list_entities with a filter to find an alternative entity, or ask the operator to adjust the entity lists." excluded))))
      (let ((state (api-call :get (format nil "/api/states/~a" entity-id))))
        (nlk:encode-json-object
         (nlk:json-object
          "entity_id" (or (nlk:json-value state :string "entity_id") entity-id)
          "state" (or (nlk:json-value state :string "state") "")
          "attributes" (or (nlk:json-value state :object "attributes") (nlk:json-object))
          "last_changed" (or (nlk:json-value state :string "last_changed") "")
          "last_updated" (or (nlk:json-value state :string "last_updated") "")))))))

;;; --- ha_list_services -------------------------------------------------------------

(defun service-entry (info)
  "One service's INFO, compacted: its description, and its fields' descriptions."
  (let ((entry (nlk:json-object "description" (or (nlk:json-value info :string "description") ""))))
    (let ((fields (nlk:json-value info :object "fields")))
      (when (and fields (plusp (hash-table-count fields)))
        (let ((described (make-hash-table :test #'equal)))
          (loop for name being the hash-keys of fields using (hash-value field)
                do (when (hash-table-p field)
                     (setf (gethash name described)
                           (or (nlk:json-value field :string "description") ""))))
          (setf (gethash "fields" entry) described))))
    entry))

(defun list-services (call)
  "The ha_list_services tool: what actions the instance offers, per domain."
  (answering
    (let* ((domain (nonblank (service-list-domain call)))
           (services (coerce (nlk:json-array (api-call :get "/api/services")) 'list))
           (services (if domain
                         (remove-if-not (lambda (entry)
                                          (string= domain (or (nlk:json-value entry :string "domain") "")))
                                        services)
                         services)))
      (nlk:encode-json-object
       (nlk:json-object
        "count" (length services)
        "domains"
        (map 'vector
             (lambda (entry)
               (let ((offered (make-hash-table :test #'equal))
                     (table (nlk:json-value entry :object "services")))
                 (when table
                   (loop for name being the hash-keys of table using (hash-value info)
                         do (setf (gethash name offered) (service-entry info))))
                 (nlk:json-object "domain" (or (nlk:json-value entry :string "domain") "")
                                  "services" offered)))
             services))))))

;;; --- ha_call_service --------------------------------------------------------------

(defun changed-states (result)
  "The states a service call changed, from either answer shape: a bare array of
states, or {\"changed_states\": [...], \"service_response\": {...}}."
  (cond ((vectorp result) (coerce result 'list))
        ((hash-table-p result) (coerce (nlk:json-array result "changed_states") 'list))
        (t '())))

(defun affected-entities (result)
  (map 'vector (lambda (state)
                 (nlk:json-object "entity_id" (or (nlk:json-value state :string "entity_id") "")
                                  "state" (or (nlk:json-value state :string "state") "")))
       (changed-states result)))

(defparameter +target-states+
  '(("turn_off" "off") ("lock" "locked") ("unlock" "unlocked") ("open" "open")
    ("open_cover" "open") ("close_cover" "closed") ("open_valve" "open")
    ("close_valve" "closed"))
  "Services that name the state they put an entity in, and the state that
satisfies them.")

(defun requested-state-reached (service state)
  "Whether STATE is what SERVICE asked for; NIL when the service names no
state, so nothing can be compared."
  (cond ((string= service "toggle") nil)
        ((string= service "turn_on")
         (and state (not (member state '("off" "unavailable" "unknown") :test #'string=))))
        (t (let ((targets (cdr (assoc service +target-states+ :test #'string=))))
             (and targets (member state targets :test #'string=) t)))))

(defun nothing-changed (domain service entity-id)
  "Why DOMAIN.SERVICE changed nothing for ENTITY-ID, said as the model can act
on it: a missing entity, another domain, or the state it asked for already."
  (multiple-value-bind (body status) (exchange :get (format nil "/api/states/~a" entity-id))
    (cond ((and (integerp status) (= status 404))
           (format nil "~a.~a affected no entities: ~a does not exist in Home Assistant. Check the exact entity_id with ha_list_entities."
                   domain service entity-id))
          ((or (null status) (>= status 400))
           (format nil "~a.~a affected no entities, and ~a could not be checked (HTTP ~a)."
                   domain service entity-id status))
          (t (let* ((state (ignore-errors (nlk:decode-json body)))
                    (actual (or (nlk:json-value state :string "entity_id") ""))
                    (current (or (nlk:json-value state :string "state") ""))
                    (actual-domain (subseq actual 0 (or (position #\. actual) (length actual)))))
               (cond ((and (plusp (length actual-domain))
                           (not (string= actual-domain domain))
                           (not (string= domain "homeassistant")))
                      (format nil "~a.~a affected no entities: ~a exists but is in the '~a' domain, not '~a'. A service call only affects entities of its own domain - call ~a.~a instead."
                              domain service entity-id actual-domain domain actual-domain service))
                     ((requested-state-reached service current)
                      (format nil "~a was already in state '~a' - nothing to change." entity-id current))
                     (t (format nil "~a.~a was accepted; Home Assistant reported no state change for ~a (state '~a'). Check with ha_get_state if the change matters."
                                domain service entity-id current))))))))

(defun service-answer (path payload)
  "PATH's answer to PAYLOAD, asking for the service's response once when Home
Assistant says the service requires it (weather.get_forecasts and its like)."
  (multiple-value-bind (body status) (exchange :post path :payload payload)
    (cond ((and (integerp status) (< status 400))
           (or (ignore-errors (nlk:decode-json body)) nil))
          ((search "requires responses" (or body ""))
           (multiple-value-bind (body status)
               (exchange :post (concatenate 'string path "?return_response") :payload payload)
             (if (and (integerp status) (< status 400))
                 (or (ignore-errors (nlk:decode-json body)) nil)
                 (fail "~a answered HTTP ~a~@[: ~a~]" path status (answer-detail body)))))
          (t (fail "~a answered HTTP ~a~@[: ~a~]" path status (answer-detail body))))))

(defun call-service (call)
  "The ha_call_service tool: one service, with the guards Home Assistant lacks."
  (answering
    (let* ((domain (nlk:trimmed (or (service-call-domain call) "")))
           (service (nlk:trimmed (or (service-call-service call) "")))
           (entity-id (nonblank (service-call-entity-id call)))
           (data (service-call-data call)))
      (unless (ppcre:scan +name-pattern+ domain)
        (return-from call-service (refuse "invalid domain format: ~s" domain)))
      (unless (ppcre:scan +name-pattern+ service)
        (return-from call-service (refuse "invalid service format: ~s" service)))
      (when (member domain +blocked-domains+ :test #'string=)
        (return-from call-service
          (refuse "service domain ~s is blocked for security: it runs code on the Home Assistant host or reaches the local network. Blocked domains: ~{~a~^, ~}."
                  domain +blocked-domains+)))
      (when (and entity-id (not (ppcre:scan +entity-id-pattern+ entity-id)))
        (return-from call-service (refuse "invalid entity_id format: ~s" entity-id)))
      (let ((payload (if entity-id
                          (nlk:copy-json-object (or data (nlk:json-object)) "entity_id" entity-id)
                          data)))
        (let ((violation (service-call-violation domain service payload)))
          (when violation
            (return-from call-service (refuse "refused by operator config: ~a" violation))))
        (let* ((path (format nil "/api/services/~a/~a" domain service))
               (result (service-answer path payload))
               (answer (nlk:json-object "success" t
                                         "service" (format nil "~a.~a" domain service)
                                         "affected_entities" (affected-entities result)))
               (response (and (hash-table-p result)
                              (nlk:json-value result :object "service_response"))))
          (when response
            (setf (gethash "service_response" answer) response))
          (when (and entity-id (null response) (zerop (length (changed-states result))))
            ;; Home Assistant answers only the states that changed: an empty
            ;; answer is not a success yet, and the model must not assume one.
            (setf (gethash "success" answer) nil
                  (gethash "error" answer) (nothing-changed domain service entity-id)
                  (gethash "nothing_changed" answer) t))
          (nlk:encode-json-object answer))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell home-assistant
  (:section ("homeassistant")
    (:guide "a long-lived access token from your instance (Profile, Security, Long-Lived Access Tokens) named in token_env or kept in token_file; url is the instance's base URL, scheme included; the entity lists are optional and decide what the tools may list, read and control; next step: ask for the kitchen lights")
    (:one-of "token_env" "token_file")
    ("url" :string :default "http://homeassistant.local:8123"
     :doc "the instance's base URL, http:// or https:// included")
    ("token_env" :env
     :doc "the environment variable holding a long-lived access token (HASS_TOKEN)")
    ("token_file" :path
     :doc "a file holding a long-lived access token, which the file's own permissions guard")
    ("entity_allowlist" :list
     :doc "entity ids or patterns (climate.*, sensor.office_*) the four tools are limited to; empty, everything the token can reach")
    ("entity_denylist" :list
     :doc "entity ids or patterns the four tools always exclude; it wins over the allowlist, and a denied entity is refused before any request")
    ("max_entities" :integer :min 1
     :doc "the cap on ha_list_entities answers; unset, no cap")
    ("timeout" :integer :default 15 :min 1
     :doc "how long one Home Assistant request may take, in seconds"))
  (:settings #'read-settings)
  (:help :home-assistant
         "home-assistant: ha_list_entities, ha_get_state, ha_list_services and ha_call_service read and control Home Assistant devices"
         *manual*)
  (:tool "ha_list_entities" (nlk:contract 'entity-list) #'list-entities)
  (:tool "ha_get_state" (nlk:contract 'state-read) #'read-state)
  (:tool "ha_list_services" (nlk:contract 'service-list) #'list-services)
  (:tool "ha_call_service" (nlk:contract 'service-call) #'call-service))
