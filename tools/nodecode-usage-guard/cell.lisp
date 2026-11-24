;;;; cell.lisp --- the section, the reading, the verdict, and the two surfaces.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What it does. A subscription (Claude, or a ChatGPT/Codex plan) is spent
;;;; by usage windows: five hours, a week. Nodecode has no pre-call veto, so
;;;; this cell is the reading and the warning: one tool the model calls to
;;;; see where the windows stand, /usage-guard for the operator, and a note
;;;; added to the first round of a turn once a window passes a warn level, so
;;;; a long run winds down before the wall instead of after it.
;;;;
;;;; Where the numbers come from. The two endpoints the original names, read
;;;; with the sign-in the app beside it already holds: the Claude Code OAuth
;;;; usage endpoint with that login's access token, and the Codex usage
;;;; endpoint with the Codex login's token. A token is sent to its own
;;;; provider and nowhere else; nothing else on the machine is read.
;;;;
;;;; What is kept. One JSON file in this home, holding per source the last
;;;; good windows and when they were read: never a token, never a raw body.
;;;; A reading is reused while it is younger than max_age_s and never re-read
;;;; more often than min_interval_s, so a burst of turns is one request; a
;;;; failed read never erases the last good windows, which stay usable for
;;;; stale_grace_s.

(in-package #:nodecode-usage-guard)

;;; --- the sources this guard knows -------------------------------------------
;;; Two, both published by the app that uses them and read with that app's own
;;; sign-in. A third would be one more entry here and one parse function.

(defparameter +sources+
  '(("claude"
     :url "https://api.anthropic.com/api/oauth/usage"
     :env "CLAUDE_CODE_OAUTH_TOKEN"
     :credentials "~/.claude/.credentials.json"
     :token ("claudeAiOauth" "accessToken"))
    ("codex"
     :url "https://chatgpt.com/backend-api/wham/usage"
     :env "CODEX_ACCESS_TOKEN"
     :credentials "~/.codex/auth.json"
     :token ("tokens" "access_token")
     :account ("tokens" "account_id")))
  "The sources, by name: the endpoint, the environment variable a token may
come from, and the file and path its own app keeps the token at.")

(defparameter +claude-windows+
  '(("five_hour" . "5 hour") ("seven_day" . "7 day")
    ("seven_day_opus" . "opus 7 day") ("seven_day_sonnet" . "sonnet 7 day"))
  "The windows the Claude usage answer publishes, and what the report calls them.")

(defparameter +codex-windows+
  '(("primary_window" . "primary") ("secondary_window" . "secondary"))
  "The windows the Codex usage answer publishes under its rate limit.")

(defparameter +timeout+ 20
  "Seconds one usage read may take: past it the last reading stands.")

;;; --- small readers over a decoded JSON object --------------------------------

(defun json-member (object key)
  "KEY in the decoded JSON OBJECT, a hash table, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun number-member (object key)
  "KEY in OBJECT as a number, or NIL."
  (let ((value (json-member object key)))
    (and (realp value) value)))

(defun string-member (object key)
  "KEY in OBJECT as a string, or NIL."
  (let ((value (json-member object key)))
    (and (stringp value) value)))

(defun object-member (object &rest path)
  "The object PATH walks to from OBJECT, or NIL."
  (reduce (lambda (object key) (json-member object key)) path :initial-value object))

(defun truthy (value)
  "VALUE as a boolean, the way a JSON answer means one."
  (and value (not (equal value :false)) (not (eql value 0)) t))

;;; --- what a reading is: a window, a stamp, a state --------------------------

(defun window (label percent reset)
  "One usage window: what it is called, how much of it is used, and when it
resets (universal time, or NIL)."
  (list :label label :percent (float percent) :reset reset))

(defun window-label (entry fallback)
  "ENTRY's own length in words when it says one -- `5 hours' -- else FALLBACK."
  (let ((minutes (number-member entry "window_minutes")))
    (cond ((and minutes (>= minutes 60))
           (format nil "~:d hour~:p" (round minutes 60)))
          (minutes (format nil "~:d minute~:p" minutes))
          (t fallback))))

(defun parse-reset (value)
  "VALUE -- an epoch second, or the ISO timestamp a usage answer carries --
as universal time, or NIL."
  (handler-case
      (cond ((realp value) (if (> value 1000000000) (round (- value 2208988800)) (round value)))
            ((stringp value) (parse-iso-time value))
            (t nil))
    (error () nil)))

(defun parse-iso-time (text)
  "TEXT, `2026-01-02T03:04:05[.000]Z' or with a `+HH:MM' offset, as universal time."
  (when (>= (length text) 19)
    (let* ((year (parse-integer text :start 0 :end 4))
           (month (parse-integer text :start 5 :end 7))
           (day (parse-integer text :start 8 :end 10))
           (hour (parse-integer text :start 11 :end 13))
           (minute (parse-integer text :start 14 :end 16))
           (second (parse-integer text :start 17 :end 19))
           (offset (iso-offset-seconds text)))
      (- (encode-universal-time second minute hour day month year 0) offset))))

(defun iso-offset-seconds (text)
  "The zone TEXT names after its seconds -- `Z' is 0 -- in seconds east."
  (let ((tail (subseq text 19)))
    (cond ((zerop (length tail)) 0)
          ((char= (char tail 0) #\Z) 0)
          ((not (member (char tail 0) '(#\+ #\-) :test #'char=)) 0)
          (t (let* ((sign (if (char= (char tail 0) #\+) 1 -1))
                    (hours (parse-integer tail :start 1 :end 3 :junk-allowed t))
                    (minutes (and (> (length tail) 4)
                                  (parse-integer tail :start 4 :end 6 :junk-allowed t))))
               (* sign (+ (* 60 (or hours 0)) (or minutes 0))))))))

(defun now ()
  "Universal time, as every stamp in the cache is kept."
  (get-universal-time))

;;; --- the cache: this home's one JSON file -----------------------------------

(defparameter +cache-version+ 1)

(defun expand-path (text)
  "TEXT with a leading ~/ read against the operator's home."
  (if (and (stringp text) (uiop:string-prefix-p "~/" text))
      (uiop:native-namestring (merge-pathnames (subseq text 2) (user-homedir-pathname)))
      text))

(defun cache-path ()
  "Where the readings are kept: the section's cache_file, else this home's
own usage-guard/usage.json."
  (let ((named (ignore-errors (setting :cache-file))))
    (if (and (stringp named) (plusp (length named)))
        (expand-path named)
        (uiop:native-namestring (nlk:home "usage-guard/usage.json")))))

(defun read-cache ()
  "The cache as decoded JSON, an object; an empty one when there is none or
it cannot be read."
  (handler-case
      (let ((path (cache-path)))
        (if (probe-file path)
            (let ((object (nlk:decode-json (uiop:read-file-string path))))
              (if (hash-table-p object) object (nlk:json-object)))
            (nlk:json-object)))
    (error () (nlk:json-object))))

(defun write-cache (object)
  "OBJECT as the cache file, written whole or not at all."
  (handler-case
      (let* ((path (cache-path))
             (temporary (concatenate 'string path ".new")))
        (ensure-directories-exist path)
        (with-open-file (stream temporary :direction :output :if-exists :supersede
                                         :if-does-not-exist :create :external-format :utf-8)
          (write-string (nlk:encode-json-object object) stream))
        (uiop:rename-file-overwriting-target (pathname temporary) (pathname path))
        t)
    (error () nil)))

(defun cache-source (cache name)
  "NAME's record in CACHE, or NIL."
  (json-member (json-member cache "sources") name))

(defun stored-windows (record)
  "RECORD's windows as plists, dropping any the contract does not hold."
  (let ((raw (json-member record "windows")))
    (loop for entry across (if (vectorp raw) raw #())
          for percent = (number-member entry "used_percent")
          when (and percent (<= 0 percent 100))
            collect (window (or (string-member entry "label") "window") percent
                             (parse-reset (number-member entry "reset_at"))))))

(defun windows-json (windows)
  "WINDOWS as the cache keeps them: a vector of objects."
  (coerce (loop for w in windows
                collect (nlk:json-object "label" (getf w :label)
                                          "used_percent" (getf w :percent)
                                          "reset_at" (or (getf w :reset) 0)))
          'vector))

(defun store-source (name &key windows fetched-at failure)
  "NAME's record with WINDOWS and FETCHED-AT, and FAILURE when this read
failed; the other sources are kept as they are."
  (let* ((cache (read-cache))
         (sources (or (json-member cache "sources") (nlk:json-object))))
    (setf (gethash "version" cache) +cache-version+
          (gethash "sources" cache) sources
          (gethash name sources)
          (nlk:json-object "fetched_at" (or fetched-at 0)
                           "windows" (windows-json windows)
                           "failure" (or failure "")
                           "failed_at" (if failure (now) 0)))
    (write-cache cache)))

;;; --- the reading ------------------------------------------------------------

(defun http-get (url headers)
  "GET URL with HEADERS => (values BODY STATUS)."
  (multiple-value-bind (body status)
      (nlk:http :get url :headers headers :timeout +timeout+ :connect-timeout +timeout+)
    (values (if (stringp body) body (ignore-errors (nlk:body-text body))) status)))

(defvar *fetch* #'http-get
  "The one HTTP call a reading makes: a function of URL and HEADERS answering
(values BODY STATUS). A test binds it, so no test touches the network.")

(defun source-value (name key)
  "This section's override for source NAME's KEY, else the built-in one."
  (let* ((source (assoc name +sources+ :test #'string=))
         (override (ignore-errors
                    (setting (intern (string-upcase (format nil "~a-~a" name key)) :keyword)))))
    (if (and (stringp override) (plusp (length override)))
        override
        (getf (rest source) key))))

(defun source-url (name)
  "NAME's usage endpoint."
  (string-right-trim "/" (source-value name :url)))

(defun source-token (name)
  "=> (values TOKEN ACCOUNT): the sign-in the app beside this one already
holds -- from the environment variable the section may name, else the file
its own login wrote. NIL when there is none."
  (let* ((source (assoc name +sources+ :test #'string=))
         (env (source-value name :env))
         (path (expand-path (source-value name :credentials)))
         (from-env (and env (uiop:getenv env))))
    (if (and from-env (plusp (length from-env)))
        (values from-env nil)
        (handler-case
            (let* ((object (and (probe-file path) (nlk:decode-json (uiop:read-file-string path))))
                   (token (and object (apply #'object-member object (getf (rest source) :token))))
                   (account (and object (getf (rest source) :account)
                                 (apply #'object-member object (getf (rest source) :account)))))
              (if (and (stringp token) (plusp (length token)))
                  (values token (and (stringp account) account))
                  nil))
          (error () nil)))))

(defun source-headers (name token account)
  "The headers a usage read of NAME is made with: the token, and what the
source's own client sends beside it."
  (if (string= name "claude")
      (list (cons "authorization" (format nil "Bearer ~a" token))
            (cons "anthropic-beta" "oauth-2025-04-20")
            (cons "anthropic-version" "2023-06-01"))
      (append (list (cons "authorization" (format nil "Bearer ~a" token)))
              (when account (list (cons "chatgpt-account-id" account))))))

(defun claude-windows (object)
  "The windows a Claude usage answer carries."
  (loop for (key . label) in +claude-windows+
        for entry = (json-member object key)
        for percent = (number-member entry "utilization")
        when percent
          collect (window label percent
                          (parse-reset (or (json-member entry "resets_at")
                                           (json-member entry "reset_at"))))))

(defun codex-windows (object)
  "The windows a Codex usage answer carries, under its rate limit."
  (let* ((limit (json-member object "rate_limit"))
         (windows (loop for (key . label) in +codex-windows+
                        for entry = (json-member limit key)
                        for percent = (number-member entry "used_percent")
                        when percent
                          collect (window (window-label entry label) percent
                                          (parse-reset (or (json-member entry "resets_at")
                                                           (json-member entry "reset_at")))))))
    (if (and (null windows) (truthy (json-member limit "limit_reached")))
        (list (window "server" 100.0 nil))
        windows)))

(defun source-windows (name object)
  "The windows NAME's answer OBJECT publishes."
  (if (string= name "claude") (claude-windows object) (codex-windows object)))

(defun fetch-source (name)
  "One usage read of NAME => (values WINDOWS NIL), else (values NIL REASON)."
  (handler-case
      (multiple-value-bind (token account) (source-token name)
        (if (null token)
            (values nil (format nil "no ~a sign-in found" name))
            (multiple-value-bind (body status)
                (funcall *fetch* (source-url name) (source-headers name token account))
              (cond ((not (and (integerp status) (< status 300)))
                     (values nil (format nil "the usage endpoint answered HTTP ~a"
                                         (or status "?"))))
                    (t (let* ((object (nlk:decode-json (or body "")))
                              (windows (and (hash-table-p object) (source-windows name object))))
                         (if windows
                             (values windows nil)
                             (values nil "the answer carried no usage window"))))))))
    ((or error sb-sys:deadline-timeout) (condition)
      (values nil (princ-to-string condition)))))

(defun snapshot (name &key force)
  "NAME's evidence, read or reused, never signaling. => a plist of :windows
the windows, :fetched-at when the good reading was made, :state one of
:fresh :stale :unavailable, and :reason what went wrong."
  (let* ((record (cache-source (read-cache) name))
         (windows (and record (stored-windows record)))
         (fetched (and record (let ((at (number-member record "fetched_at")))
                                (and at (plusp at) (round at)))))
         (age (and fetched (max 0 (- (now) fetched))))
         (floor (max 0 (or (ignore-errors (setting :min-interval-s)) 0)))
         (max-age (max floor (max 0 (or (ignore-errors (setting :max-age-s)) 0))))
         (grace (max 0 (or (ignore-errors (setting :stale-grace-s)) 0))))
    (labels ((answer (state reason)
               (list :windows windows :fetched-at fetched :state state :reason reason)))
      (cond
        ;; A reading young enough, or one this guard may not replace yet.
        ((and windows (not force) age (< age max-age)) (answer :fresh nil))
        ((and windows (not force) age (< age floor)) (answer :fresh nil))
        (t
         (multiple-value-bind (fresh reason) (fetch-source name)
           (cond
             (fresh
              (store-source name :windows fresh :fetched-at (now))
              (setf windows fresh fetched (now))
              (answer :fresh nil))
             (t
              (store-source name :windows windows :fetched-at fetched :failure reason)
              (if (and windows age (<= age grace))
                  (answer :stale reason)
                  (answer :unavailable (or reason "no reading yet")))))))))))

;;; --- the verdict --------------------------------------------------------------

(defun threshold ()
  "The percent used at which a window counts as spent."
  (or (ignore-errors (setting :threshold-percent)) 100))

(defun warn-levels ()
  "The levels, highest first, that earn one note in a turn."
  (sort (remove nil (mapcar (lambda (value) (ignore-errors (parse-integer (string value))))
                            (coerce (or (ignore-errors (setting :warn-percents)) '()) 'list)))
        #'>))

(defun hits (windows)
  "The windows at or over the threshold, highest first."
  (sort (remove-if-not (lambda (w) (>= (getf w :percent) (threshold))) windows)
        #'> :key (lambda (w) (getf w :percent))))

(defun reset-text (reset)
  "RESET as the report says it, or NIL when unknown."
  (when reset
    (let ((seconds (- reset (now))))
      (if (<= seconds 0)
          "reset due, not confirmed"
          (format nil "resets in ~@[~dh ~]~dmin" (floor seconds 3600) (floor (mod seconds 3600) 60))))))

(defun age-text (fetched)
  "FETCHED as the report says it."
  (if (null fetched)
      "never read"
      (let ((seconds (max 0 (- (now) fetched))))
        (cond ((< seconds 90) (format nil "read ~ds ago" seconds))
              ((< seconds 5400) (format nil "read ~dm ago" (floor seconds 60)))
              (t (format nil "read ~dh ago" (floor seconds 3600)))))))

(defun source-line (name snapshot)
  "One source's line: its verdict and its windows. => (values LINE STATE)"
  (let* ((windows (getf snapshot :windows))
         (blocked (hits windows)))
    (values
     (format nil "  ~a: ~a (~a)"
             name
             (cond (blocked (format nil "AT THE LIMIT -- ~a at ~,1f% used~@[, ~a~]; stop starting work on it"
                                     (getf (first blocked) :label) (getf (first blocked) :percent)
                                     (reset-text (getf (first blocked) :reset))))
                   ((null windows)
                    (format nil "cannot confirm -- ~a" (or (getf snapshot :reason) "no reading")))
                   (t (format nil "~{~a~^, ~}"
                              (loop for w in windows
                                    collect (format nil "~a ~,1f%~@[ (~a)~]"
                                                    (getf w :label) (getf w :percent)
                                                    (reset-text (getf w :reset)))))))
             (age-text (getf snapshot :fetched-at)))
     (cond (blocked :blocked)
           ((null windows) :unavailable)
           (t :ok)))))

(defun gated-sources ()
  "The sources this section watches: those it names that the guard knows how
to read. An empty list is a section that names none, which start refuses."
  (let ((named (coerce (or (ignore-errors (setting :sources)) '()) 'list)))
    (remove-if-not (lambda (name) (and (stringp name) (assoc name +sources+ :test #'string=)))
                   named)))

(defun report (&key force)
  "Every watched source's windows and the verdict over them. => (values TEXT
 VERDICT NOTE): VERDICT is :ok, :blocked or :unverified, NOTE the one line
the model acts on."
  (running-settings)
  (let* ((readings (loop for name in (gated-sources) collect (cons name (snapshot name :force force))))
         (lines (loop for (name . snapshot) in readings
                      collect (source-line name snapshot)))
         (states (loop for (name . snapshot) in readings
                       collect (nth-value 1 (source-line name snapshot))))
         (names (format nil "~{~a~^, ~}" (mapcar #'car readings)))
         (fail-open (ignore-errors (setting :fail-open)))
         (verdict (cond ((member :blocked states) :blocked)
                        ((and (member :unavailable states) (not fail-open)) :unverified)
                        (t :ok)))
         (note (case verdict
                 (:blocked (format nil "Do not start new work on ~a until a window resets: switch model with /model, or wait." names))
                 (:unverified (format nil "Remaining allowance on ~a is unconfirmed: keep the next requests small, or set fail_open to send anyway." names))
                 (t "Every watched window has room left."))))
    (values (format nil "usage-guard ~a~%~{~a~%~}~a  (~a; at the limit from ~d% used)"
                    (string-upcase (string verdict)) lines note names (round (threshold)))
            verdict note)))

(defun status ()
  "The report, as an eval form says it."
  (values (report)))

(defun windows ()
  "The windows and their percentages as data, for eval: one plist per source."
  (loop for name in (gated-sources)
        for snapshot = (snapshot name)
        collect (list :source name :state (getf snapshot :state)
                      :fetched-at (getf snapshot :fetched-at)
                      :windows (getf snapshot :windows))))

;;; --- the note a turn sees -------------------------------------------------------

(defvar *warned* (make-hash-table :test #'equal)
  "SESSION and NOTE -> T, so one turn is told a note once.")

(defun warn-note ()
  "The sentence a turn near the wall is told, from the cache only, or NIL."
  (let ((levels (warn-levels)))
    (when levels
      (let ((worst (loop for name in (gated-sources)
                         for snapshot = (snapshot name)
                         for peak = (loop for w in (getf snapshot :windows)
                                          maximize (getf w :percent) into best
                                          finally (return best))
                         when peak collect (cons name peak))))
        (when worst
          (let* ((level (loop for (name . peak) in worst maximize peak))
                 (crossed (find-if (lambda (l) (>= level l)) levels)))
            (when crossed
              (format nil "[usage-guard] ~{~a~^; ~} -- a subscription window is over ~d% used. ~
Wind the current thread down and wait for the reset before starting anything long.~%"
                      (loop for (name . peak) in worst
                            when (>= peak crossed)
                              collect (format nil "~a at ~,1f%" name peak))
                      (round crossed)))))))))

(defun warn-at-round (op next)
  "A :ROUND point hook: on a turn's first round, one note rides its request
for each warn level crossed. Never the turn's failure."
  (prog1 (funcall next op)
    (handler-case
        (when (and (getf op :turn) (eql 0 (or (getf op :round) 0)))
          (let ((note (warn-note)))
            (when note
              (let ((key (format nil "~a|~a" (getf op :session-id) note)))
                (unless (gethash key *warned*)
                  (setf (gethash key *warned*) t)
                  (nlk:record-turn-context-prompt (getf op :turn) :text note))))))
      (error () nil))))

;;; --- /usage-guard ---------------------------------------------------------------

(defun run-slash (args session-id)
  "/usage-guard: the report as the operator reads it; `check' reads now
instead of reusing a reading."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member verb '("" "status") :test #'string=) (nth-value 0 (report)))
          ((equal verb "check") (nth-value 0 (report :force t)))
          (t (format nil "usage-guard: ~a is not one of its choices: status, check" verb)))))

;;; --- the manual -------------------------------------------------------------------

(defparameter +manual+
  "The usage guard watches the subscription windows behind a Claude and a ChatGPT/Codex plan and
says when one is used up, so a long run stops before it spends paid extra usage.
  (usage_guard (:action \"status\"))   every watched window: what is used, when it resets, and how
      old the reading is. It reads again only when the reading is older than max_age_s, and never
      more often than min_interval_s, so asking again is cheap. The first line is the verdict.
  (usage_guard (:action \"check\"))    the same, asking for a read now if the floor allows it.
  (usage-guard:report)                the same text from eval; (usage-guard:windows) the data.
  A turn's first round is told once when a window passes a warn level (80% and 90% by default):
finish the current thread, wait for the reset, or switch model with /model.
  The numbers come from each app's own sign-in -- Claude Code's login for claude, the Codex login
for codex -- and that token goes to its provider's usage endpoint only. With no readable sign-in,
or an endpoint that does not answer, the source is `cannot confirm': keep the next requests
small, or set fail_open to send anyway. Nodecode does not veto the request itself; the guard is
the reading and the warning.
ERROR: USAGE-GUARD-ERROR names the problem, never a token."
  "What (help :usage-guard) answers while the cell runs.")

;;; --- the tool's boundary -----------------------------------------------------------

(nlk:define-contract usage-guard-request
  (:doc "Read the subscription usage windows behind the providers you are running on: what percent
of each is used, when it resets, and whether the guard can confirm it at all. Call it before a
long run, or when the operator asks how much is left. The first line is the verdict: BLOCKED at
or over the threshold, UNVERIFIED when no fresh reading could be had, OK otherwise.")
  ("action" :text :options '("status" "check") :default "status"
   :doc "status reuses a reading younger than max_age_s and refreshes an older one if the read floor
allows; check asks for a read now"))

(defun run-tool (request)
  "The usage_guard tool: the report, as text."
  (values (report :force (equal "check" (usage-guard-request-action request)))))

(nle:define-cell usage-guard
  (:section ("usage-guard")
    (:guide "sources names what to watch (claude, codex) and is required; no key of your own: the guard reads the sign-in Claude Code and Codex already have; threshold_percent 100 means the limit itself, lower it for a margin; /usage-guard shows where things stand")
    ("sources" :list :required t
     :doc "which subscriptions to watch: claude (a Claude Code login) and codex (a ChatGPT/Codex login)")
    ("threshold_percent" :integer :default 100 :min 1
     :doc "a window at or over this percent used is at the limit; 100 is the subscription's own wall")
    ("warn_percents" :list :default (vector "80" "90")
     :doc "levels at which one note rides the next turn's first round, so a run winds down; empty disables")
    ("max_age_s" :integer :default 60 :min 0
     :doc "how old a reading may be before it is read again")
    ("min_interval_s" :integer :default 180 :min 0
     :doc "least seconds between two reads of one endpoint, shared by every turn; the endpoints have a small hourly budget")
    ("stale_grace_s" :integer :default 900 :min 0
     :doc "how long the last good reading stands while a refresh fails, before the source counts as unconfirmed")
    ("fail_open" :boolean :default nil
     :doc "report OK from a stale or missing reading instead of unconfirmed")
    ("cache_file" :string :default ""
     :doc "where readings are kept; empty is this home's usage-guard/usage.json")
    ("claude_base_url" :string :default "" :doc "the Claude usage endpoint; empty is the published one")
    ("codex_base_url" :string :default "" :doc "the Codex usage endpoint; empty is the published one")
    ("claude_credentials" :string :default ""
     :doc "the file holding the Claude Code login; empty is the one Claude Code writes")
    ("codex_credentials" :string :default ""
     :doc "the file holding the Codex login; empty is the one Codex writes")
    ("claude_token_env" :string :default ""
     :doc "an environment variable holding the Claude token; empty is CLAUDE_CODE_OAUTH_TOKEN")
    ("codex_token_env" :string :default ""
     :doc "an environment variable holding the Codex token; empty is CODEX_ACCESS_TOKEN"))
  (:tool "usage_guard" (nlk:contract 'usage-guard-request) 'run-tool)
  (:help :usage-guard "usage-guard: how much of the Claude and Codex subscription windows is used, when each resets, and a note when one is nearly spent" +manual+)
  (:command "usage-guard" 'run-slash
            :description "Subscription usage windows: what is used, when each resets"
            :argument-hint "[status|check]")
  (:hook :round #'warn-at-round)
  (:start (lambda ()
            (unless (gated-sources)
              (fail "usage-guard watches no source: name claude, codex, or both in sources"))
            nil)))
