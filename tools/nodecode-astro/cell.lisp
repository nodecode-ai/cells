;;;; cell.lisp --- the ephemeris behind one MCP server, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin had no code: an MCP server declaration
;;;; (a hosted streamable-HTTP endpoint, no auth) and one skill that taught
;;;; the model when to use it. This cell speaks that server's JSON-RPC
;;;; itself through nlk:http -- initialize, tools/call, and an answer that is
;;;; either a JSON object or a one-event stream -- and reaches the same 24
;;;; tools: three named ones for the charts people ask for, and one that
;;;; calls any of the others by name. The skill's rules are (help :astro).
;;;;
;;;; Config, a sibling top-level key:
;;;;   "astro": {"endpoint": "https://astro-agent.dev/mcp",
;;;;             "credits_token_file": "~/.nodecode/secrets/astro-credits",
;;;;             "timeout_seconds": 60, "max_chars": 8000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-astro)

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "Astro Agents answers exact astrology and planetary timing instead of an estimate: charts
from the NASA/JPL DE440 ephemeris, deterministic and SHA-256 verifiable, no model involved.
Use the tools whenever a request depends on where the planets were or are: a birth or natal
chart, sun, moon and rising sign, aspects and transits, synastry; a Vedic kundli, divisional
chart, nakshatra, Vimshottari dasha, dosha, yoga, panchang, kundli matching, KP; or planetary
timing for market work (heliocentric positions, declination, Gann lines, an eclipse and
ingress calendar). Never compute these yourself: time zones, historical daylight saving,
sidereal time and the ayanamsa are where charts go wrong.
  astro_chart    a Western natal chart, planet positions, or aspects
  astro_kundli   a Vedic kundli, or all sixteen divisional charts
  astro_sky      the sky now and the events of the next days, no birth data needed
  astro_call     any of the server's 24 tools by name, with its own arguments
How to ask:
1. Pass the local clock time as datetime, 1990-05-15T14:30, with latitude and longitude in
   decimal degrees (north and east positive, so west longitudes and south latitudes are
   negative). Do not convert to UTC: the server resolves the zone and its historical offset.
2. A birth time that is unknown is a date alone: the chart is cast at noon with a
   TIME_UNKNOWN warning, and the ascendant and houses are not reliable then. Say so.
3. Two people (synastry, gun_milan) go through astro_call as person_a and person_b, each
   with its own datetime, latitude and longitude.
4. Quote positions, signs and scores exactly as returned; mention meta.result_sha256 when the
   person wants a result they can verify.
5. One free call per client, on the cheap tools only (resolve_birth_time, planet_positions,
   sky_today, aspects, nakshatras, panchang); every other tool is paid and answers
   payment_required with its price. Say the price, do not retry, do not compute the chart.
6. Western tools are tropical, Vedic tools sidereal (Lahiri, and the KP ayanamsa for
   kp_chart); do not mix the two systems in one answer. During a daylight-saving fall-back
   hour pass ambiguous_time earlier or later.
This is data for the person's own analysis, never a trading signal or a forecast."
  "What (help :astro) answers while the cell runs: the skill's rules, in one place.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client offers at initialize; the server may answer another.")

(defparameter +client-version+ "0.1.0")

;;; --- the server's 24 tools, named once -----------------------------------------

(defparameter +tools+
  '("resolve_birth_time" "planet_positions" "sky_today" "aspects"
    "natal_chart" "transits" "synastry" "planetary_lines" "astro_event_calendar"
    "sky_analogues" "token_chart" "token_synastry" "nakshatras" "panchang"
    "kundli" "kundli_full" "vimshottari_dasha" "doshas" "gun_milan"
    "vedic_report" "kp_chart" "yogas" "astro_catalog" "verify_result_hash")
  "Every tool the server lists, so a name the model asks for is checked here first.")

(defparameter +free-tools+ '("resolve_birth_time" "planet_positions" "sky_today"
                             "aspects" "nakshatras" "panchang")
  "The cheap tools a client gets one free call of.")

;;; --- answers, and how a refusal reaches the model ---------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on, as the call's failure."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal or a dead network becomes that text, never an
unhandled error."
  `(handler-case (progn ,@body)
     (astro-error (condition)
       (nle:failure (format nil "ERROR: ~a" (astro-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: astro: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun clip (text limit)
  "TEXT, cut at LIMIT characters with a note of how much is left."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a... (~d more characters)"
                (subseq text 0 limit) (- (length text) limit)))))

;;; --- reading the section ---------------------------------------------------------

(defun read-token-file (path)
  "The token in PATH's first non-empty line, or NIL when the file is not there."
  (let ((truename (probe-file path)))
    (when truename
      (handler-case
          (with-open-file (in truename :external-format :utf-8)
            (loop for line = (read-line in nil nil)
                  while line
                  for text = (nonblank line)
                  when text return text))
        (error () nil)))))

(defun read-settings (values table)
  "The declared VALUES, with defaults filled in and the credits token resolved
once, here: the section's, else the file it names."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :endpoint) (nonblank (getf values :endpoint))
          (getf plist :timeout-seconds) (or (getf values :timeout-seconds) 60)
          (getf plist :max-chars) (or (getf values :max-chars) 8000))
    (let* ((inline (nonblank (getf values :credits-token)))
           (file (nonblank (getf values :credits-token-file)))
           (from-file (and file (read-token-file file))))
      (setf (getf plist :credits-token) (or inline from-file)))
    plist))

(defun credits-source ()
  "Where the credits token comes from, in words: never the token itself."
  (if (nonblank (setting :credits-token))
      "the astro section"
      "none: the free calls, then payment per call"))

(defun check-config ()
  "Refuse a section that cannot reach the server, naming the member to set."
  (unless (nonblank (setting :endpoint))
    (error 'nlk:config-refusal
           :detail "astro: endpoint is empty; set it to https://astro-agent.dev/mcp (no account or key is needed)"))
  (unless (search "https://" (setting :endpoint))
    (error 'nlk:config-refusal
           :detail "astro: endpoint must be an https address, https://astro-agent.dev/mcp")))

;;; --- the wire: MCP over streamable HTTP -------------------------------------------
;; One POST per JSON-RPC message, and an answer that is either the bare object
;; or a one-event stream (data: {...}). The server here hands no session id and
;; answers plain JSON; a server that hands one gets it echoed on every request,
;; and a 404 means the session it gave is gone.

(defvar *session-id* nil
  "The Mcp-Session-Id this image was given at initialize, or NIL.")

(defvar *negotiated* nil
  "The MCP revision the server answered at initialize, sent with every request.")

(defvar *initialized* nil
  "Whether this image signed in to the server since the cell started.")

(defvar *request-id* 0)

(defun reset-session ()
  (setf *session-id* nil *negotiated* nil *initialized* nil *request-id* 0))

(defun next-id ()
  (incf *request-id*))

(defun header-value (headers name)
  "NAME's value in the response HEADERS nlk:http answers, or NIL."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-downcase name) headers)))
        ((consp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun request-headers ()
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream"))
          (when (nonblank (setting :credits-token))
            (list (cons "x-credits-token" (setting :credits-token))))
          (when *session-id* (list (cons "mcp-session-id" *session-id*)))
          (when *negotiated* (list (cons "mcp-protocol-version" *negotiated*)))))

(defun post-json (body)
  "One HTTPS POST of BODY to the server => (values TEXT STATUS HEADERS)."
  (nlk:http :post (setting :endpoint)
            :headers (request-headers)
            :content body
            :timeout (setting :timeout-seconds)
            :connect-timeout (min 20 (setting :timeout-seconds))))

(defun stream-json (text)
  "The last JSON object a one-event stream body carries, or NIL."
  (let ((last nil))
    (loop for line in (uiop:split-string (or text "") :separator '(#\Newline))
          do (let ((line (string-right-trim '(#\Return) line)))
               (when (and (> (length line) 5) (string-equal "data:" (subseq line 0 5)))
                 (let ((payload (nlk:trimmed (subseq line 5))))
                   (when (and (plusp (length payload)) (char= (char payload 0) #\{))
                     (setf last payload))))))
    last))

(defun rpc-object (text)
  "The JSON-RPC object in TEXT: a bare body, or the last data line of a stream."
  (let* ((text (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
         (text (cond ((null text) nil)
                     ((zerop (length text)) nil)
                     ((char= (char text 0) #\{) text)
                     (t (stream-json text)))))
    (when (and text (plusp (length text)))
      (ignore-errors (nlk:decode-json text)))))

(defun rpc-error-text (object)
  "The words a JSON-RPC error object carries, or NIL."
  (let ((error (and (hash-table-p object) (nlk:json-value object :object "error"))))
    (when error
      (or (nlk:json-value error :text "message")
          (nlk:one-line (nlk:encode-json-object error))))))

(defun fail-on-status (status object)
  (cond
    ((equal status 402)
     (fail "astro: the server wants a payment for this call (HTTP 402)~@[: ~a~]. A credits token in the astro section pays for it, and the cheap tools have one free call." (rpc-error-text object)))
    ((member status '(401 403))
     (fail "astro: the server refused this client (HTTP ~a)~@[: ~a~]. Check the endpoint in the astro section." status (rpc-error-text object)))
    (t
     (fail "astro's server answered HTTP ~a~@[: ~a~]" status (rpc-error-text object)))))

(defun send-rpc (method params)
  "One JSON-RPC request of METHOD with PARAMS => (values OBJECT STATUS)."
  (let ((id (next-id)))
    (multiple-value-bind (text status headers)
        (post-json (nlk:encode-json-object
                    (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                     "params" (or params (nlk:make-json-object)))))
      (let ((session (header-value headers "mcp-session-id")))
        (when session
          (setf *session-id* (if (stringp session) session (princ-to-string session)))))
      (values (rpc-object text) status))))

(defun send-note (method)
  "The one no-answer message MCP wants after initialize; a 202 is the whole answer."
  (ignore-errors
    (post-json (nlk:encode-json-object
                (nlk:json-object "jsonrpc" "2.0" "method" method
                                 "params" (nlk:make-json-object))))
    t))

(defun initialize ()
  (multiple-value-bind (object status)
      (send-rpc "initialize"
                (nlk:json-object "protocolVersion" +protocol-version+
                                  "capabilities" (nlk:make-json-object)
                                  "clientInfo" (nlk:json-object "name" "nodecode-astro"
                                                                 "version" +client-version+)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail-on-status status object))
    (setf *negotiated*
          (or (nlk:json-value (nlk:json-value object :object "result")
                              :text "protocolVersion")
              +protocol-version+))
    (send-note "notifications/initialized")
    (setf *initialized* t)
    t))

(defun ensure-session ()
  (unless *initialized* (initialize)))

(defun mcp-call (method &optional params)
  "One MCP method, signing in first and once more after a session the server
forgot => the result object. A refusal or a dead network signals ASTRO-ERROR,
which every verb turns into the model's answer."
  (ensure-session)
  (multiple-value-bind (object status) (send-rpc method params)
    (when (equal status 404)
      (reset-session)
      (ensure-session)
      (multiple-value-bind (again again-status) (send-rpc method params)
        (setf object again status again-status)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail-on-status status object))
    (let ((reason (rpc-error-text object)))
      (when reason
        (fail "astro's server refused ~a: ~a" method reason)))
    (or (nlk:json-value object :object "result")
        (nlk:make-json-object))))

;;; --- what one tools/call answers ---------------------------------------------------

(defun content-text (result)
  "The text parts of one tools/call RESULT, joined by a newline."
  (with-output-to-string (out)
    (let ((first t))
      (loop for part across (nlk:json-array result "content")
            when (equal "text" (nlk:json-value part :text "type"))
              do (let ((text (nlk:json-value part :string "text")))
                   (when text
                     (unless first (terpri out))
                     (setf first nil)
                     (write-string text out)))))))

(defun result-header (structured)
  "One line naming what answered and the hash that proves it, or NIL."
  (let* ((endpoint (nlk:json-value structured :text "endpoint"))
         (meta (nlk:json-value structured :object "meta"))
         (hash (nlk:json-value meta :text "result_sha256")))
    (when (or endpoint hash)
      (format nil "~@[~a~]~@[ - result_sha256 ~a~]~@[ (verify with astro_call verify_result_hash)~]"
              endpoint hash (and hash t)))))

(defun render-result (result)
  "One tools/call RESULT as the model reads it => (values TEXT ERROR-P)."
  (let* ((structured (nlk:json-value result :object "structuredContent"))
         (errorp (and (nlk:json-value result :boolean "isError") t))
         (body (or (and structured (nlk:encode-json-object structured))
                   (content-text result)))
         (header (and structured (result-header structured))))
    (values (cond ((and header body) (format nil "~a~%~a" header body))
                  (header header)
                  (body body)
                  (t (nlk:encode-json-object result)))
            errorp)))

(defun call-on-server (tool arguments)
  "One tools/call of TOOL on the server => (values TEXT ERROR-P)."
  (render-result
   (mcp-call "tools/call"
             (nlk:json-object "name" tool
                               "arguments" (or arguments (nlk:make-json-object))))))

(defun payment-hint (text)
  "The words to add when the server refused a call for payment, or NIL."
  (when (search "payment" text :test #'char-equal)
    (format nil "~%The server wants paying for this call. A credits token in the astro section ~
(credits_token, or credits_token_file) pays for it call by call; the one free call per client ~
only covers ~{~a~^, ~}. Do not retry the same call, and do not compute the chart yourself."
            +free-tools+)))

(defun answer (tool arguments)
  "One server tool call as the model reads it: its text, clipped, or the failure it reported."
  (multiple-value-bind (text errorp) (call-on-server tool arguments)
    (if errorp
        (nle:failure (format nil "ERROR: ~a~@[~a~]" text (payment-hint text)))
        (clip text (setting :max-chars)))))

;;; --- arguments -----------------------------------------------------------------------

(defun argument (arguments key)
  (nlk:json-value arguments :text key))

(defun number-argument (arguments key)
  (nlk:json-value arguments :number key))

(defun put-arguments (payload arguments keys)
  "Copy every present KEY of ARGUMENTS onto PAYLOAD, unchanged."
  (dolist (key keys)
    (let ((value (nlk:json-value arguments :any key)))
      (when value (setf (gethash key payload) value))))
  payload)

(defparameter +time-keys+ '("datetime" "date" "time" "timezone" "time_basis" "ambiguous_time")
  "What a chart and a moment have in common; the server resolves the zone from the place.")

(defun put-place (payload arguments)
  "ARGS' latitude and longitude, when both were given => (values LAT LON)."
  (let ((latitude (number-argument arguments "latitude"))
        (longitude (number-argument arguments "longitude")))
    (when latitude (setf (gethash "latitude" payload) latitude))
    (when longitude (setf (gethash "longitude" payload) longitude))
    (values latitude longitude)))

(defun put-options (payload arguments)
  (let ((options (nlk:json-value arguments :object "options")))
    (when options (setf (gethash "options" payload) options)))
  payload)

;;; --- the four tools --------------------------------------------------------------------

(defparameter +chart-schema+
  (list :description
        (concatenate 'string
                     "A Western chart for one moment and place, from the NASA/JPL DE440 ephemeris. "
                     "kind natal (the default) is a birth chart: planets in signs and houses, the "
                     "ascendant, aspects. kind positions is where the planets are, with speed and "
                     "retrogradation. kind aspects is the aspect list alone. Pass the LOCAL clock "
                     "time as datetime (1990-05-15T14:30) with latitude and longitude in decimal "
                     "degrees, north and east positive; the server resolves the time zone and its "
                     "historical offset, so never convert to UTC. A birth time that is unknown is "
                     "a date alone: the chart is cast at noon and the ascendant is not reliable.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "kind" (nlk:make-json-object
                                "type" "string"
                                "enum" (vector "natal" "positions" "aspects")
                                "description" "natal (a birth chart), positions, or aspects")
                       "datetime" (nlk:make-json-object "type" "string"
                                                          "description" "local date and time, 1990-05-15T14:30")
                       "date" (nlk:make-json-object "type" "string"
                                                      "description" "the date alone, when the time is unknown")
                       "latitude" (nlk:make-json-object "type" "number"
                                                         "description" "decimal degrees, north positive")
                       "longitude" (nlk:make-json-object "type" "number"
                                                          "description" "decimal degrees, east positive")
                       "timezone" (nlk:make-json-object "type" "string"
                                                         "description" "an IANA zone, only when the place is not enough")
                       "ambiguous_time" (nlk:make-json-object
                                          "type" "string"
                                          "enum" (vector "earlier" "later")
                                          "description" "which side of a daylight-saving fall-back hour")
                       "options" (nlk:make-json-object
                                  "type" "object"
                                  "description" "calculation settings: house_system, zodiac, ayanamsa, aspect orbs")))))

(defun chart (arguments)
  "ASTRO_CHART: natal_chart, planet_positions or aspects, by kind."
  (answering
    (let* ((arguments (or arguments (nlk:make-json-object)))
           (kind (string-downcase (or (argument arguments "kind") "natal")))
           (tool (cond ((member kind '("natal" "chart" "birth") :test #'string=) "natal_chart")
                       ((member kind '("positions" "planets" "ephemeris") :test #'string=) "planet_positions")
                       ((string= kind "aspects") "aspects")
                       (t nil))))
      (unless tool
        (return-from chart (refuse "kind is natal, positions or aspects, not ~a" kind)))
      (let ((payload (put-arguments (nlk:json-object) arguments +time-keys+)))
        (multiple-value-bind (latitude longitude) (put-place payload arguments)
          (when (equal tool "natal_chart")
            (unless (and latitude longitude)
              (return-from chart
                (refuse "a natal chart needs the place: latitude and longitude in decimal degrees, ~
and the local datetime of the birth")))))
        (put-options payload arguments)
        (answer tool payload)))))

(defparameter +sky-schema+
  (list :description
        (concatenate 'string
                     "The sky now and what is coming: planet positions and retrogrades, the Moon "
                     "phase, then the ingresses, stations, lunations and aspects of the next days. "
                     "No birth data is needed; add latitude and longitude to place it. Cheap, and "
                     "one of the tools a client gets a free call of.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "days" (nlk:make-json-object "type" "integer"
                                                     "minimum" 1 "maximum" 90 "default" 30
                                                     "description" "how many days ahead to list events, 30 unless given")
                       "datetime" (nlk:make-json-object "type" "string"
                                                          "description" "local date and time to read the sky at, now unless given")
                       "date" (nlk:make-json-object "type" "string" "description" "the date alone")
                       "latitude" (nlk:make-json-object "type" "number" "description" "decimal degrees, north positive")
                       "longitude" (nlk:make-json-object "type" "number" "description" "decimal degrees, east positive")
                       "options" (nlk:make-json-object "type" "object"
                                                         "description" "zodiac settings: tropical, or sidereal with an ayanamsa")))))

(defun sky (arguments)
  "ASTRO_SKY: sky_today, which needs no birth data."
  (answering
    (let* ((arguments (or arguments (nlk:make-json-object)))
           (payload (put-arguments (nlk:json-object) arguments +time-keys+)))
      (setf (gethash "days" payload)
            (or (nlk:json-value arguments :integer "days") 30))
      (put-place payload arguments)
      (put-options payload arguments)
      (answer "sky_today" payload))))

(defparameter +kundli-schema+
  (list :description
        (concatenate 'string
                     "A Vedic (Jyotish) birth chart, sidereal with the Lahiri ayanamsa: lagna and the "
                     "nine grahas with their nakshatras, D1 and D9. full true asks for all sixteen "
                     "divisional charts (D1 to D60) in one call. Pass the local birth time as "
                     "datetime and the place as latitude and longitude in decimal degrees, north and "
                     "east positive; the server resolves the time zone and its historical offset.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "full" (nlk:make-json-object "type" "boolean"
                                                      "description" "all sixteen divisional charts, not just D1 and D9")
                       "datetime" (nlk:make-json-object "type" "string"
                                                          "description" "local date and time, 1990-05-15T14:30")
                       "date" (nlk:make-json-object "type" "string"
                                                      "description" "the date alone, when the time is unknown")
                       "latitude" (nlk:make-json-object "type" "number" "description" "decimal degrees, north positive")
                       "longitude" (nlk:make-json-object "type" "number" "description" "decimal degrees, east positive")
                       "options" (nlk:make-json-object "type" "object"
                                                         "description" "ayanamsa or house system, when the usual Parashari conventions do not apply"))
         "required" (vector "latitude" "longitude"))))

(defun kundli (arguments)
  "ASTRO_KUNDLI: kundli, or kundli_full for every divisional chart."
  (answering
    (let* ((arguments (or arguments (nlk:make-json-object)))
           (payload (put-arguments (nlk:json-object) arguments +time-keys+)))
      (multiple-value-bind (latitude longitude) (put-place payload arguments)
        (unless (and latitude longitude)
          (return-from kundli
            (refuse "a kundli needs the birth place: latitude and longitude in decimal degrees"))))
      (put-options payload arguments)
      (answer (if (nlk:json-value arguments :boolean "full") "kundli_full" "kundli") payload))))

(defparameter +call-schema+
  (list :description
        (concatenate 'string
                     "Any of the Astro Agents server's 24 tools, called by name with its own arguments "
                     "object. Use it for what the named tools do not cover: transits and synastry, "
                     "panchang, vimshottari_dasha, doshas, yogas, kp_chart, gun_milan, vedic_report, "
                     "nakshatras, resolve_birth_time, the financial-astrology tools (planetary_lines, "
                     "astro_event_calendar, sky_analogues, token_chart, token_synastry), astro_catalog "
                     "for every tool's price, and verify_result_hash to check a returned result_sha256. "
                     "The arguments object is the server tool's own: datetime plus latitude and "
                     "longitude for a chart, person_a and person_b for a comparison of two people, "
                     "start and end for a calendar. A paid tool answers payment_required with its price.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "tool" (nlk:make-json-object
                                "type" "string"
                                "enum" (apply #'vector +tools+)
                                "description" "the server tool to call, by its own name")
                       "arguments" (nlk:make-json-object
                                     "type" "object"
                                     "description" "that tool's arguments, as its schema describes them"))
         "required" (vector "tool"))))

(defun any-tool (arguments)
  "ASTRO_CALL: one server tool by name, its arguments passed through."
  (answering
    (let* ((arguments (or arguments (nlk:make-json-object)))
           (tool (argument arguments "tool"))
           (payload (nlk:json-value arguments :object "arguments")))
      (cond ((not (nonblank tool))
             (refuse "tool is required: one of the server's 24 names, such as transits or panchang"))
            ((not (member tool +tools+ :test #'string=))
             (refuse "~a is not one of the server's tools; call astro_catalog for the list" tool))
            (t (answer tool (or payload (nlk:make-json-object))))))))

;;; --- the operator's command ----------------------------------------------------------

(defun tool-list-text ()
  "The server's tools, and which of them a free call covers."
  (format nil "astro: ~d tools on the server.~%Free call (one per client): ~{~a~^, ~}.~%Paid, ~
price from astro_catalog: ~{~a~^, ~}.~%Call any of them with astro_call; a chart needs datetime, ~
latitude and longitude."
          (length +tools+)
          +free-tools+
          (set-difference (copy-list +tools+) +free-tools+ :test #'string=)))

(defun chart-from-line (line)
  "A natal chart from TIME LAT LON; anything else is refused with the usage."
  (let ((parts (remove "" (uiop:split-string line :separator '(#\Space #\Tab)) :test #'string=)))
    (if (< (length parts) 3)
        (refuse "usage: /astro for today's sky, /astro tools for the list, or /astro 1990-05-15T14:30 48.8566 2.3522 for a chart")
        (let ((latitude (ignore-errors (float (read-from-string (second parts)) 1.0d0)))
              (longitude (ignore-errors (float (read-from-string (third parts)) 1.0d0))))
          (if (and (numberp latitude) (numberp longitude))
              (chart (nlk:make-json-object "kind" "natal"
                                            "datetime" (first parts)
                                            "latitude" latitude
                                            "longitude" longitude))
              (refuse "the place is latitude and longitude in decimal degrees, such as 48.8566 2.3522"))))))

(defun run-command (args session-id)
  "One /astro invocation: today's sky, the tool list, or a chart from a time and place."
  (declare (ignore session-id))
  (let ((wanted (nlk:trimmed (or args ""))))
    (cond ((zerop (length wanted)) (sky (nlk:make-json-object)))
          ((member wanted '("tools" "list" "help") :test #'string-equal) (tool-list-text))
          ((string-equal wanted "credits")
           (format nil "astro: credits come from ~a." (credits-source)))
          (t (chart-from-line wanted)))))

;;; --- the cell --------------------------------------------------------------------------

(nle:define-cell astro
  (:section ("astro")
    (:guide "set endpoint to the service's MCP address, https://astro-agent.dev/mcp (no account or API key; the cheap tools have one free call); a prepaid credits token in credits_token, or in the file credits_token_file names, pays for the rest; /astro shows today's sky")
    ("endpoint" :string :required t
     :doc "the Astro Agents MCP endpoint, https://astro-agent.dev/mcp")
    ("credits_token" :secret
     :doc "a prepaid credits token, sent as the X-Credits-Token header")
    ("credits_token_file" :path
     :doc "a file holding that token instead of the token member")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one call's ceiling: a full chart or a year of events takes seconds")
    ("max_chars" :integer :default 8000 :min 500
     :doc "how much of one answer the model reads"))
  (:settings 'read-settings)
  (:help :astro
         "astro: exact Western and Vedic charts, and planetary timing, from an astrology MCP server - (help :astro)"
         +primer+)
  (:command "astro" 'run-command
            :description "Today's sky, the server's tools, or a chart from a time and place"
            :argument-hint "TIME LAT LON, tools, credits"
            :session nil)
  (:tool "astro_chart" +chart-schema+ 'chart)
  (:tool "astro_sky" +sky-schema+ 'sky)
  (:tool "astro_kundli" +kundli-schema+ 'kundli)
  (:tool "astro_call" +call-schema+ 'any-tool)
  (:start (lambda ()
            (check-config)
            (reset-session)
            (nle:on-stop #'reset-session))))
