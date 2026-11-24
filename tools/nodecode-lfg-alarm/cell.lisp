;;;; cell.lisp --- the LFG Alarm cell: its section, its manual, its start.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin is one mcp.json entry and one skill. The entry is client.lisp;
;;;; the skill is the manual below, which (help :lfg-alarm) answers while the
;;;; cell runs and every request's help section names in one line.
;;;;
;;;; Config, a top-level key:
;;;;   "lfg-alarm": {"token_env": "LFG_ALARM_KEY"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-lfg-alarm)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "LFG Alarm is a social alarm clock: an alarm rings on a phone in the LFG Alarm app, at a time
or when something happens, and a shared alarm rings on every phone that joined it. Ask for one
when a message would not be loud enough. The person needs the LFG Alarm app, signed in; the key
this cell holds is theirs, and every call goes to that account.

What the tools are (the server's own list, normally; /lfg-alarm prints it):
  set_time_alarm   a time, once or on chosen days.
  set_event_alarm  an event page (Luma, Partiful, Eventbrite, Ticketmaster, Posh), rung at the
                   start or 30, 60 or 120 minutes before.
  set_post_alarm   an X account posting what the person describes; a Twitch or YouTube channel
                   going live or posting.
  set_price_alarm  a token's price, or a percent move; find the token's id first.
  set_trader_alarm a trader, a token's trades by contract address, or a wallet's buys and sells.
  set_drop_alarm   an artist's SoundCloud drop; find the artist first, many share a name.
  set_watch_alarm  a page or a topic Rocco watches on a schedule, rung when the person's
                   condition comes true.
  set_webhook_alarm a private link any system can call to ring it; ring_alarm rings on demand.
  set_sun_alarm    sunrise, golden hour, sunset, dusk, moonrise at a place, or a sky event.
  set_tide_alarm   low or high tide, a negative low, a king tide, at the nearest station.
  list_my_alarms, update_alarm, turn_alarm_on_off, delete_alarm  what the account holds.
  open_link, join_alarm, list_friends, list_riders, invite and join requests  the people side.
  search_library, set_media, add_media_from_url  the sound and the look an alarm wears.
  sun_times, tide_times  read-only.
Ask nothing the server does not list: when a tool is not there, say so instead of pretending an
alarm is set.

How to use them well:
  - Pass the person's IANA time zone. For a shared alarm across cities, ask whether everyone
    rings at the same moment (ring_at together, the default) or each at their own local time.
  - Reach is private (just them), friends (people named) or public (anyone with the link, in
    Discover); default to private unless other people are mentioned.
  - Adding people: their LFG Alarm username, a friend group by name, or anyone by email. Never
    guess who someone is: the first call only lists who would be added, so read the names back,
    get a yes, then call again with people_confirmed true. Up to 20 at a time.
  - Read a contract address or an artist link back to the person before it is used; names and
    tickers match copycats.
  - Name the alarm for the moment (\"Airport, car at 5:15\") and put the details in the note.
  - A create answers the alarm's link (lfg.gm/a/<code>), a share picture with a QR code and a
    ready-to-send message: give all three back to the person.
  - A tool that says the plan does not cover it means just that: tell the person to look at
    lfg.gm for plans, never invent a price.
  - Time and event alarms ring through silent mode and Focus on iPhone; post, price and webhook
    alarms ring full screen on Android and arrive as a time-sensitive alert on iPhone."
  "What (help :lfg-alarm) answers while the cell runs.")

;;; --- reading the section ---------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the key the section names: read once, here."
  (declare (ignore table))
  (let ((url (string-right-trim "/" (or (getf values :url) "")))
        (env (nonblank (getf values :token-env)))
        (file (nonblank (getf values :token-file)))
        (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of token_env and token_file, not both"))
    (unless (or (uiop:string-prefix-p "https://" url) (uiop:string-prefix-p "http://" url))
      (nlk:config-error "url ~s has no http:// or https:// scheme: set it to the LFG Alarm server, https://mcp.lfg.gm" url))
    (setf (getf plist :url) url
          (getf plist :token)
          (cond (env (or (nonblank (nle::credential-env env))
                         (nlk:config-error "token_env names ~a, which is unset or empty" env)))
                (file (or (nonblank (ignore-errors (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "token_file ~a cannot be read, or is empty" file)))
                (t (nlk:config-error "no key: make a personal key in the LFG Alarm app's settings, then name the environment variable holding it in token_env, or the file holding it in token_file"))))
    plist))

;;; --- the slash command ------------------------------------------------------------

(defun tool-line (tool)
  "One tool as the operator reads it: its name and the first sentence of what it does."
  (let* ((description (getf tool :description))
         (stop (and description (position #\. description))))
    (format nil "  ~a~@[ - ~a~]" (getf tool :name)
            (and description (plusp (length description))
                 (subseq description 0 (and stop (1+ stop)))))))

(defun run-slash (args session-id)
  "The tools this server offers, or one of them by name; and a reconnect."
  (declare (ignore session-id))
  (handler-case
      (let* ((wanted (nlk:trimmed (or args "")))
             (names (register-remote-tools))
             (listed (remote-tools)))
        (cond ((zerop (length names))
               (format nil "lfg-alarm: ~a lists no tools." (setting :url)))
              ((plusp (length wanted))
               (let ((tool (find wanted listed :key (lambda (tool) (getf tool :name))
                                          :test #'string-equal)))
                 (if tool
                     (format nil "~a~%~a" (tool-line tool) (getf tool :description))
                     (format nil "lfg-alarm: no tool called ~a. It offers:~%~{~a~%~}"
                             wanted (mapcar #'tool-line listed)))))
              (t (format nil "lfg-alarm: ~a tool~:p from ~a~%~{~a~%~}~%The model calls them by name; (help :lfg-alarm) says what they are for."
                         (length names) (setting :url) (mapcar #'tool-line listed)))))
    (lfg-alarm-error (condition)
      (format nil "lfg-alarm: ~a" (lfg-alarm-error-detail condition)))
    (error (condition)
      (format nil "lfg-alarm: ~a" (nlk:one-line (princ-to-string condition))))))

;;; --- the cell --------------------------------------------------------------------

(defun note-start-failure (detail)
  (nle:notice (format nil "lfg-alarm: the server did not answer at start (~a); /lfg-alarm tries again"
                       detail)
              :level :warning :key "nodecode-lfg-alarm"))

(nle:define-cell lfg-alarm
  (:section ("lfg-alarm")
    (:guide "make a personal key in the LFG Alarm app's settings, then name the variable holding it in token_env (or the file holding it in token_file); url is the server, https://mcp.lfg.gm; next step: ask for an alarm")
    (:one-of "token_env" "token_file")
    ("url" :string :default "https://mcp.lfg.gm"
     :doc "the LFG Alarm MCP server, https:// or http:// included")
    ("token_env" :env
     :doc "the environment variable holding a personal key from the LFG Alarm app's settings")
    ("token_file" :path
     :doc "a file holding a personal key, which the file's own permissions guard")
    ("timeout" :integer :default 60 :min 5
     :doc "how long one request to the server may take, in seconds")
    ("max_output" :integer :default 6000 :min 500
     :doc "how much of a tool's answer one result carries, in characters"))
  (:settings #'read-settings)
  (:help :lfg-alarm
         "lfg-alarm: the LFG Alarm tools put a real alarm on a phone; /lfg-alarm lists what the server offers"
         *manual*)
  (:command "lfg-alarm" 'run-slash
            :description "The LFG Alarm tools this server offers, and a reconnect"
            :argument-hint "[tool name]")
  (:start (lambda ()
            (let ((names (handler-case (register-remote-tools)
                           (lfg-alarm-error (condition)
                             (note-start-failure (lfg-alarm-error-detail condition))
                             '())
                           (error (condition)
                             (note-start-failure (nlk:one-line (princ-to-string condition)))
                             '()))))
              (when names (nle:notice nil :key "nodecode-lfg-alarm"))
              (nle:on-stop (lambda ()
                             (unregister-remote-tools)
                             (nle:notice nil :key "nodecode-lfg-alarm"))))))

)
