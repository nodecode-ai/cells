;;;; cell.lisp --- FateTell's BaZi charts, almanac and saved charts, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Agent Plugins package in RussGuo/fatetell-mcp (see
;;;; NOTICE). The original carries no code of its own: it points a host at
;;;; FateTell's remote MCP server and adds the fatetell-bazi skill on how to
;;;; read the results. Here the same server is reached over the same wire --
;;;; MCP Streamable HTTP, initialize then tools/call, the answer read out of
;;;; result.content[] -- with one tool per server tool, /fatetell for the
;;;; operator, and the reading rules as (help :fatetell).
;;;;
;;;; Casting a chart and reading the almanac need no account. The rest need
;;;; a FateTell sign-in, which in a cell is an OAuth access token in the
;;;; section: the original let its host run the sign-in page, and a cell has
;;;; no browser flow of its own.

(defpackage #:nodecode-fatetell
  (:use #:cl)
  (:export #:fatetell-error #:fatetell-error-detail))

(in-package #:nodecode-fatetell)

(nlk:define-peripheral fatetell :not-running t)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this cell speaks, as the original's hosts do.")

(defparameter +primer+
  "FateTell computes; you interpret. Every stem and branch a FateTell tool returns is
deterministic, from FateTell's own rule set: quote it, explain it, never invent the rest.
  fatetell_chart {date, timezone, gender, hemisphere, time?}   a birth's BaZi chart
  fatetell_almanac {start_date?, days?, timezone?, hemisphere?}   stems, branches, lunar dates, solar terms, 1 to 31 days, no chart and no account
  fatetell_saved {chart_id?}        the account's charts, or one of them; try it before re-casting
  fatetell_judgment {chart_id | date...}   Day Master strength and favorable elements
  fatetell_fortune {chart_id, date?}      one day for a saved chart: energy, relations, do/avoid
  fatetell_calendar {chart_id, count?, granularity?}   the same for the days or months ahead
  /fatetell account                 who is signed in, the membership, the caps left (the operator's command)
  fatetell_create {display_name, gender, birth_date, birthplace}   save a chart; the only write
Casting and the almanac work with no account; the rest answer a sign-in challenge, which the
section's OAuth access token answers. For \"my chart\" read fatetell_saved first, then
fatetell_saved with the chart_id -- never re-cast a saved chart from birth data.
Reading: use the en field beside each Chinese term, keep 八字 characters as they are, and
treat ten_god, hidden_stems, life_stage and nayin as given. Ask for the missing piece once
when a birth has no time or hemisphere; an assumed noon hour makes the hour pillar
provisional, so say so. Pass use_true_solar_time and longitude only when the user asks for
true solar time. Present a judgment as FateTell's verdict with its rule version, and an
energy as high, medium or low -- there is no score. Relations (clash, combination,
punishment, harm, break) are tendencies of a day, not events: no deaths, divorces, illness,
accidents or lucky years, and nothing medical, financial or legal decided on a chart. Quote
do and avoid items as given, answer in the language the user asked in, and say plainly what
a membership_required or rate_limited answer needs. When the user names someone else's
birth, cast or save their chart only if they know and agree."
  "What (help :fatetell) answers while the cell runs: the original's skill, condensed.")

;;; --- the section, read back ---------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the access token, read once, here: the token written
inline, else the variable access_token_env names. No token is required -- the
chart and the almanac answer without one -- and the account tools say what they
need when the server challenges them."
  (declare (ignore table))
  (let* ((env (nonblank (getf values :access-token-env)))
         (inline (nonblank (getf values :access-token)))
         (token (or inline (and env (nonblank (uiop:getenv env)))))
         (url (string-right-trim "/" (or (getf values :mcp-url) "")))
         (plist (copy-list values)))
    (when (and env (null token))
      (nlk:config-error "access_token_env names ~a, but nothing is set there: put a FateTell OAuth access token in it, or clear access_token_env to use the tools that need no account" env))
    (unless (or (uiop:string-prefix-p "https://" url) (uiop:string-prefix-p "http://" url))
      (nlk:config-error "mcp_url ~s has no http:// or https:// scheme: set it to https://mcp.fatetell.com/mcp" url))
    (setf (getf plist :token) token
          (getf plist :url) url)
    plist))

;;; --- the MCP wire ---------------------------------------------------------------

(defvar *session* nil
  "The MCP session this process opened, as (URL TOKEN SESSION-ID): FateTell is
stateless and issues no session id; a server that issues one keeps it here.")

(defun unwrap-sse (text)
  "TEXT with streamable-HTTP framing removed: the payload of its event's data
lines, joined, or TEXT itself when it carries none. The endpoint answers a POST
as text/event-stream, as the original's hosts expect."
  (let* ((text (and (stringp text) (nlk:trimmed text)))
         (lines (and text (uiop:split-string text :separator '(#\Newline))))
         (data (loop for line in lines
                     for body = (string-left-trim '(#\Return #\Space #\Tab) line)
                     when (uiop:string-prefix-p "data:" body)
                       collect (string-left-trim '(#\Space) (subseq body 5)))))
    (if data (format nil "~{~a~^~%~}" data) (or text ""))))

(defun decode-body (answer)
  "ANSWER, an HTTP body or a notification's empty one, decoded; an empty or
unreadable one is an empty object."
  (let ((text (unwrap-sse answer)))
    (if (plusp (length (nlk:trimmed text)))
        (or (ignore-errors (nlk:decode-json text)) (nlk:json-object))
        (nlk:json-object))))

(defun header-value (headers name)
  "HEADERS -- an alist as NLK:HTTP answers it -- NAME's value, case-insensitively."
  (loop for (key . value) in headers
        when (and (stringp key) (string-equal key name))
          return value))

(defun headers-for (&optional session)
  "The request headers: JSON in, JSON or an event stream out, the section's
token when it has one, and SESSION when one was issued."
  (let ((headers (list (cons "content-type" "application/json")
                        (cons "accept" "application/json, text/event-stream"))))
    (when (setting :token)
      (push (cons "authorization" (format nil "Bearer ~a" (setting :token))) headers))
    (when session
      (push (cons "mcp-session-id" session) headers))
    headers))

(defun post-json (payload &optional session)
  "PAYLOAD posted to the section's endpoint => (values DECODED STATUS SESSION-ID).
A failed dial signals, so the caller's refusal is the one the model reads."
  (multiple-value-bind (text status headers)
      (nlk:http :post (setting :url)
                :headers (headers-for session)
                :content (nlk:encode-json-object payload)
                :timeout (setting :timeout-seconds)
                :connect-timeout (min 10 (setting :timeout-seconds)))
    (values (decode-body text) status (header-value headers "mcp-session-id"))))

(defun content-text (result)
  "RESULT's content[].text, the text blocks joined, else its structuredContent
as JSON: what FateTell's answer says to a person."
  (let ((joined (format nil "~{~a~^~%~}"
                         (remove "" (loop for item across (or (nlk:json-array result "content") #())
                                          when (and (hash-table-p item)
                                                    (equal "text" (nlk:json-value item :string "type")))
                                            collect (or (nlk:json-value item :string "text") ""))
                                 :test #'equal))))
    (or (nonblank joined)
        (let ((structured (nlk:json-value result :object "structuredContent")))
          (and structured (nlk:encode-json-object structured))))))

(defun answer-problem (answer status)
  "ANSWER's own words for a failed call -- its JSON-RPC error, or its result's
isError text -- or a line naming STATUS when it says nothing."
  (let* ((error (nlk:json-value answer :object "error"))
         (result (nlk:json-value answer :object "result")))
    (or (and error (nonblank (nlk:json-value error :string "message")))
        (and result
             (eq t (gethash "isError" result))
             (nonblank (content-text result)))
        (format nil "HTTP ~a" status))))

(defun http-reason (status answer)
  "Why a call answered STATUS, in words a model can act on."
  (if (member status '(401 403))
      "FateTell needs a signed-in account for this tool: put an OAuth access token in the section's access_token (or in the variable access_token_env names), then try again. Chart casting and the almanac need no account."
      (format nil "the FateTell server answered HTTP ~a~@[: ~a~]" status
              (and (hash-table-p answer) (answer-problem answer status)))))

(defun open-session ()
  "The session id the calls ride on, opening one with the handshake when the
section asks for it => (values SESSION-ID REASON); a server that issues no id
is stateless, and its calls go without one."
  (let ((entry (first *session*)))
    (if (and entry
             (equal (second entry) (setting :url))
             (equal (third entry) (setting :token)))
        (values (fourth entry) nil)
        (let ((answer nil) (status nil) (session nil))
          (multiple-value-setq (answer status session)
            (post-json (nlk:json-object
                        "jsonrpc" "2.0" "id" 1 "method" "initialize"
                        "params" (nlk:json-object
                                  "protocolVersion" +protocol-version+
                                  "capabilities" (nlk:json-object)
                                  "clientInfo" (nlk:json-object
                                                "name" "nodecode-fatetell"
                                                "version" "0.1.0")))))
          (let ((reason (if (and (integerp status) (< status 300))
                            (and (nlk:json-value answer :object "error")
                                 (answer-problem answer status))
                            (http-reason status answer))))
            (if reason
                (values nil reason)
                (progn
                  ;; the notification needs no answer, and a server that dislikes it is not fatal
                  (ignore-errors
                    (post-json (nlk:json-object "jsonrpc" "2.0"
                                                 "method" "notifications/initialized")
                               session))
                  (setf *session* (list (list (setting :url) (setting :token) session)))
                  (values session nil))))))))

(defun ensure-session ()
  "The session id to send with a call, or NIL: the section may turn the
handshake off for a server that answers calls directly, and a refused
handshake leaves the call to answer for itself."
  (when (setting :initialize)
    (values (open-session))))

(defun call-tool (name arguments)
  "FateTell's NAME with ARGUMENTS, a JSON object of this cell's own making
=> the text the model reads, or a FATETELL-ERROR saying what the server said."
  (let ((session (ensure-session)))
    (multiple-value-bind (answer status)
        (post-json (nlk:json-object
                    "jsonrpc" "2.0" "id" 2 "method" "tools/call"
                    "params" (nlk:json-object "name" name "arguments" arguments))
                   session)
      (unless (and (integerp status) (< status 300))
        (fail "~a" (http-reason status answer)))
      (let ((error (nlk:json-value answer :object "error")))
        (when error
          (fail "~a answered an error: ~a" name
                (or (nonblank (nlk:json-value error :string "message"))
                    (nlk:encode-json-object error)))))
      (let* ((result (nlk:json-value answer :object "result"))
             (text (and result (content-text result))))
        (cond ((null result) (fail "~a answered nothing this cell could read" name))
              ((eq t (gethash "isError" result))
               (fail "~a: ~a" name (or text "the server reported an error")))
              ((nonblank text) (nlk:trimmed text))
              (t (fail "~a answered an empty result" name)))))))

;;; --- the arguments of each call -------------------------------------------------

(defun argument (object key value)
  "OBJECT with KEY set to VALUE when VALUE is there; => OBJECT. An absent
member keeps the server's own default."
  (when value
    (setf (gethash key object) value))
  object)

(defun flag (object key value)
  "OBJECT with KEY set to a JSON boolean, whatever VALUE is: a switch the model
turned off is sent as false, not left to the server's default."
  (setf (gethash key object) (if value t :false))
  object)

(defun birth-arguments (&key date timezone gender hemisphere time calendar
                           lunar-leap-month longitude use-true-solar-time)
  "One birth as the server's `birth' object: members that are not there are
left out, so its own defaults stand, and its two switches are always sent."
  (let ((birth (nlk:json-object)))
    (argument birth "date" date)
    (argument birth "timezone" timezone)
    (argument birth "gender" gender)
    (argument birth "hemisphere" hemisphere)
    (argument birth "time" time)
    (argument birth "calendar" calendar)
    (argument birth "longitude" longitude)
    (flag birth "lunar_leap_month" lunar-leap-month)
    (flag birth "use_true_solar_time" use-true-solar-time)
    birth))

(defun chart-arguments (call)
  "A chart call's arguments: its birth data, and how to present the result."
  (let ((arguments (nlk:json-object
                    "birth" (birth-arguments
                             :date (fatetell-chart-date call)
                             :timezone (fatetell-chart-timezone call)
                             :gender (fatetell-chart-gender call)
                             :hemisphere (fatetell-chart-hemisphere call)
                             :time (fatetell-chart-time-of-birth call)
                             :calendar (fatetell-chart-calendar call)
                             :lunar-leap-month (fatetell-chart-lunar-leap-month call)
                             :longitude (fatetell-chart-longitude call)
                             :use-true-solar-time (fatetell-chart-use-true-solar-time call)))))
    (flag arguments "include_today" (fatetell-chart-include-today call))
    (argument arguments "locale" (fatetell-chart-locale call))
    arguments))

;;; --- the tools ------------------------------------------------------------------

(define-verb cast-chart (call)
  "The fatetell_chart tool: CALL's birth data cast into a BaZi chart."
  (call-tool "cast_bazi_chart" (chart-arguments call)))

(define-verb almanac (call)
  "The fatetell_almanac tool: CALL's days of Chinese almanac."
  (let ((arguments (nlk:json-object)))
    (argument arguments "start_date" (fatetell-almanac-start-date call))
    (argument arguments "days" (fatetell-almanac-days call))
    (argument arguments "timezone" (fatetell-almanac-timezone call))
    (argument arguments "hemisphere" (fatetell-almanac-hemisphere call))
    (call-tool "get_almanac" arguments)))

(defun account ()
  "The signed-in account, its membership and the caps left: /fatetell account."
  (call-tool "get_fatetell_account" (nlk:json-object)))

(define-verb saved-charts (call)
  "The fatetell_saved tool: no chart_id lists the account's charts, a chart_id
reads that one, and \"self\" reads the account holder's own."
  (let ((id (fatetell-saved-chart-id call)))
    (if (null id)
        (call-tool "list_saved_charts" (nlk:json-object))
        (let ((arguments (nlk:json-object "chart_id" id)))
          (flag arguments "include_today" (fatetell-saved-include-today call))
          (argument arguments "locale" (fatetell-saved-locale call))
          (call-tool "get_saved_chart" arguments)))))

(define-verb judgment (call)
  "The fatetell_judgment tool: FateTell's Day Master verdict and elements for a
saved chart, or for birth data given on the spot."
  (let ((arguments (nlk:json-object))
        (id (fatetell-judgment-chart-id call)))
    (if id
        (setf (gethash "chart_id" arguments) id)
        (let ((birth (birth-arguments
                      :date (fatetell-judgment-date call)
                      :timezone (fatetell-judgment-timezone call)
                      :gender (fatetell-judgment-gender call)
                      :hemisphere (fatetell-judgment-hemisphere call)
                      :time (fatetell-judgment-time-of-birth call)
                      :calendar (fatetell-judgment-calendar call)
                      :lunar-leap-month (fatetell-judgment-lunar-leap-month call)
                      :longitude (fatetell-judgment-longitude call)
                      :use-true-solar-time (fatetell-judgment-use-true-solar-time call))))
          (if (null (fatetell-judgment-date call))
              (fail "fatetell_judgment needs a chart_id, or the date, timezone, gender and hemisphere to cast from")
              (setf (gethash "birth" arguments) birth))))
    (call-tool "get_bazi_judgment" arguments)))

(define-verb daily-fortune (call)
  "The fatetell_fortune tool: one day for a saved chart."
  (let ((arguments (nlk:json-object "chart_id" (fatetell-fortune-chart-id call))))
    (argument arguments "date" (fatetell-fortune-date call))
    (argument arguments "locale" (fatetell-fortune-locale call))
    (call-tool "get_daily_fortune" arguments)))

(define-verb chart-calendar (call)
  "The fatetell_calendar tool: the days or months ahead for a saved chart."
  (let ((arguments (nlk:json-object "chart_id" (fatetell-calendar-chart-id call))))
    (argument arguments "granularity" (fatetell-calendar-granularity call))
    (argument arguments "start_date" (fatetell-calendar-start-date call))
    (argument arguments "count" (fatetell-calendar-day-count call))
    (call-tool "get_chart_calendar" arguments)))

(define-verb save-chart (call)
  "The fatetell_create tool: a chart saved to the account -- the one call here
that writes, and the only one that takes a birthplace name."
  (let ((arguments (nlk:json-object
                    "display_name" (fatetell-create-display-name call)
                    "gender" (fatetell-create-gender call)
                    "birth_date" (fatetell-create-birth-date call)
                    "birthplace" (fatetell-create-birthplace call))))
    (argument arguments "calendar" (fatetell-create-calendar call))
    (argument arguments "birth_time" (fatetell-create-birth-time call))
    (argument arguments "subject" (fatetell-create-subject call))
    (flag arguments "lunar_leap_month" (fatetell-create-lunar-leap-month call))
    (flag arguments "consent_attested" (fatetell-create-consent-attested call))
    (call-tool "create_saved_chart" arguments)))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (fatetell-error (condition)
       (nle:failure (format nil "ERROR: ~a" (fatetell-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: FateTell: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun cast-chart-tool (call) (answering (cast-chart call)))
(defun almanac-tool (call) (answering (almanac call)))
(defun saved-charts-tool (call) (answering (saved-charts call)))
(defun judgment-tool (call) (answering (judgment call)))
(defun daily-fortune-tool (call) (answering (daily-fortune call)))
(defun chart-calendar-tool (call) (answering (chart-calendar call)))
(defun save-chart-tool (call) (answering (save-chart call)))

;;; --- /fatetell -------------------------------------------------------------------

(defun status-line ()
  "What /fatetell answers with no argument: where it points, and whether an
account answers with it."
  (format nil "FateTell: ~a, ~a; /fatetell [YYYY-MM-DD [days]] gives the almanac for those days."
          (setting :url)
          (if (setting :token)
              "calling with an account token -- saved charts, judgment, fortune and calendar are open"
              "calling without a token -- charts and the almanac only")))

(defun run-command (args session-id)
  "/fatetell [YYYY-MM-DD [days]]: the almanac for those days, today's when the
date is left out. No argument, where the section points."
  (declare (ignore session-id))
  (let ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab))
                       :test #'string=)))
    (if (null words)
        (status-line)
        (if (equal (string-downcase (first words)) "account")
            (answering (account))
            (let ((arguments (nlk:json-object "start_date" (first words))))
              (when (second words)
                (let ((days (ignore-errors (parse-integer (second words)))))
                  (when days (setf (gethash "days" arguments) days))))
              (answering (call-tool "get_almanac" arguments)))))))

;;; --- the boundaries ----------------------------------------------------------------

(nlk:define-contract fatetell-chart
  (:doc "Cast a BaZi (Four Pillars) chart from birth data, computed by FateTell's engine: pillars with Ten Gods, hidden stems, Na Yin and life stage, the Day Master, every luck pillar, the palaces and today's pillars. No account is needed. Omit time when the birth time is unknown, and say the hour pillar is provisional. Hemisphere comes from the birthplace, not from the time zone.")
  ("date" :text :required t
          :doc "birth date in the birthplace's local calendar, YYYY-MM-DD")
  ("timezone" :text :required t
              :doc "IANA time zone of the birthplace, e.g. Asia/Taipei")
  ("gender" :text :required t :options '("male" "female")
            :doc "sets whether the luck pillars run forward or backward")
  ("hemisphere" :text :required t :options '("north" "south")
                :doc "the birthplace's hemisphere")
  ("time" :text :slot time-of-birth
          :doc "local clock time at the birthplace, HH:mm; omit when unknown")
  ("calendar" :text :options '("solar" "lunar")
              :doc "the date's calendar, solar by default; lunar for a Chinese lunar birthday")
  ("lunar_leap_month" :boolean
                      :doc "with calendar lunar: true when the birth was in a leap month")
  ("longitude" :number
               :doc "birthplace longitude, -180 to 180; only with true solar time")
  ("use_true_solar_time" :boolean
                          :doc "true only when the user asks for true solar time")
  ("include_today" :boolean :default t
                   :doc "include today's pillars; default true")
  ("locale" :text
            :doc "the language of the chart card, as a BCP 47 tag"))

(nlk:define-contract fatetell-almanac
  (:doc "The Chinese almanac for one to 31 consecutive days: Gregorian and lunar dates, solar terms, and the year, month and day pillars. No chart, no birth data and no account are needed.")
  ("start_date" :text
                :doc "first day, YYYY-MM-DD; default today in the time zone named")
  ("days" :integer
          :doc "how many consecutive days, 1 to 31; default 1")
  ("timezone" :text
              :doc "IANA time zone that decides the default first day; default UTC")
  ("hemisphere" :text :options '("north" "south")
                :doc "which month pillar the summary leads with; default north"))

(nlk:define-contract fatetell-saved
  (:doc "The charts saved in the connected FateTell account: their ids, names and whether they are computed. Give a chart_id to read one in full -- pillars, luck pillars, the stored judgment and today's pillars, without the birthplace. Use it before re-casting anything for a saved person. Needs a sign-in.")
  ("chart_id" :text
              :doc "a chart_id from the list, or \"self\" for the account holder's own chart")
  ("include_today" :boolean :default t
                   :doc "include today's pillars; default true")
  ("locale" :text
            :doc "the language of the chart card, as a BCP 47 tag"))

(nlk:define-contract fatetell-judgment
  (:doc "FateTell's Day Master verdict -- strong, weak, follow-strong or follow-weak -- and its ranked favorable, unfavorable and neutral elements under the stated rule set, for a saved chart or for birth data cast on the spot. Report it as FateTell's assessment, not a textbook one. A membership is needed.")
  ("chart_id" :text
              :doc "a saved chart's id, or \"self\"; takes the place of the birth fields")
  ("date" :text :doc "birth date, YYYY-MM-DD, when no chart_id is given")
  ("timezone" :text :doc "IANA time zone of that birth, e.g. Asia/Taipei")
  ("gender" :text :options '("male" "female") :doc "that birth's gender")
  ("hemisphere" :text :options '("north" "south") :doc "that birthplace's hemisphere")
  ("time" :text :slot time-of-birth :doc "local clock time, HH:mm; omit when unknown")
  ("calendar" :text :options '("solar" "lunar") :doc "the date's calendar")
  ("lunar_leap_month" :boolean :doc "with calendar lunar: a leap-month birth")
  ("longitude" :number :doc "birthplace longitude, with true solar time")
  ("use_true_solar_time" :boolean :doc "true only when the user asks for it"))

(nlk:define-contract fatetell-fortune
  (:doc "One day for a saved chart: its energy level, how the day's stems and branches relate to the natal pillars, the day's do and avoid items, and this chart's year and month pillars. A membership is needed.")
  ("chart_id" :text :required t
              :doc "a saved chart's id, or \"self\"")
  ("date" :text
          :doc "YYYY-MM-DD, within 30 days of today; default today")
  ("locale" :text :options '("zh-CN" "zh-TW" "en")
            :doc "the language of the do and avoid items"))

(nlk:define-contract fatetell-calendar
  (:doc "A saved chart's view of the days or months ahead: for each one the pillars adjusted for the chart's hemisphere, its relations with the natal pillars, month clashes and solar terms. It ranks no days -- the choice of a date is the reader's. A membership is needed.")
  ("chart_id" :text :required t
              :doc "a saved chart's id, or \"self\"")
  ("granularity" :text :options '("day" "month")
                 :doc "day by day (default) or month by month")
  ("start_date" :text
                :doc "YYYY-MM-DD to start from; default today")
  ("count" :integer :slot day-count
           :doc "days 1 to 31, default 7; months 1 to 12, default 3"))

(nlk:define-contract fatetell-create
  (:doc "Save a new chart to the connected FateTell account -- the only call here that writes, and the only one that takes a birthplace name, which FateTell uses to find the time zone and hemisphere. Do it only when the user asks to save, and with the consent of the person whose birth it is. A membership is needed.")
  ("display_name" :text :required t
                  :doc "the chart's name, 1 to 20 characters")
  ("gender" :text :required t :options '("male" "female")
            :doc "sets whether the luck pillars run forward or backward")
  ("birth_date" :text :required t
                :doc "birth date in the birthplace's local calendar, YYYY-MM-DD")
  ("birthplace" :text :required t
                :doc "the birthplace as the FateTell app takes it: a city, optionally with a country")
  ("calendar" :text :options '("solar" "lunar")
              :doc "the date's calendar; solar by default")
  ("lunar_leap_month" :boolean
                      :doc "with calendar lunar: true for a leap-month birth")
  ("birth_time" :text
                :doc "local clock time, HH:mm; omit when unknown")
  ("subject" :text
             :doc "whose chart this is, for FateTell's audit; default other")
  ("consent_attested" :boolean
                      :doc "true when the person whose birth it is knows and agrees"))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell fatetell
  (:section ("fatetell")
    (:guide "no token is needed for chart casting and the almanac; the saved charts, judgment, fortune, calendar and save need a FateTell account, whose OAuth access token goes in access_token (or in the variable access_token_env names); next step: ask for a chart or today's almanac")
    ("mcp_url" :string :default "https://mcp.fatetell.com/mcp"
               :doc "the FateTell MCP endpoint")
    ("access_token" :secret
                    :doc "a FateTell OAuth access token, written here (optional)")
    ("access_token_env" :env
                        :doc "the environment variable holding that token, when it is not written here")
    ("timeout_seconds" :integer :default 60 :min 1
                       :doc "one call's ceiling, in seconds")
    ("initialize" :boolean :default t
                  :doc "send the MCP initialize handshake before the first call"))
  (:settings #'read-settings)
  (:help :fatetell
         "fatetell: BaZi charts, the Chinese almanac and saved FateTell charts (fatetell_chart, fatetell_almanac, fatetell_saved)"
         +primer+)
  (:tool "fatetell_chart" (nlk:contract 'fatetell-chart) #'cast-chart-tool)
  (:tool "fatetell_almanac" (nlk:contract 'fatetell-almanac) #'almanac-tool)
  (:tool "fatetell_saved" (nlk:contract 'fatetell-saved) #'saved-charts-tool)
  (:tool "fatetell_judgment" (nlk:contract 'fatetell-judgment) #'judgment-tool)
  (:tool "fatetell_fortune" (nlk:contract 'fatetell-fortune) #'daily-fortune-tool)
  (:tool "fatetell_calendar" (nlk:contract 'fatetell-calendar) #'chart-calendar-tool)
  (:tool "fatetell_create" (nlk:contract 'fatetell-create) #'save-chart-tool)
  (:command "fatetell" 'run-command
            :description "FateTell: the Chinese almanac for a date"
            :argument-hint "[YYYY-MM-DD [days]]")
  (:start (lambda ()
            (nle:on-stop (lambda () (setf *session* nil))))))
