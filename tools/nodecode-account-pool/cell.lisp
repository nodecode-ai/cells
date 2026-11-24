;;;; cell.lisp --- several Claude and Codex accounts in one pool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of roqianjas's Hermes plugin hermes-account-pool
;;;; (see NOTICE). One GET per account to the vendor that owns its token --
;;;; api.anthropic.com's OAuth usage, or chatgpt.com's wham/usage -- one
;;;; tiny POST when a wake is asked for, and a plain text file under the
;;;; home for the park decisions. Nothing goes anywhere else.
;;;;
;;;; One tool (account_pool), one command (/account-pool), one manual topic,
;;;; and the Lisp verbs (account-pool:report), (account-pool:best),
;;;; (account-pool:windows "x"), (account-pool:park "x"),
;;;; (account-pool:unpark "x") and (account-pool:wake "x").

(defpackage #:nodecode-account-pool
  (:use #:cl)
  (:nicknames #:account-pool)
  (:export #:report #:windows #:best #:park #:unpark #:wake #:account-labels
           #:account-pool-error))

(in-package #:nodecode-account-pool)

(nlk:define-peripheral account-pool :not-running t)

(defvar *http* #'nlk:http
  "The one seam that leaves this image: an HTTP exchange. Tests stub it.")

(defvar *cache* (make-hash-table :test #'equal)
  "Account label -> (UNIVERSAL-TIME . SNAPSHOT), while the cell runs.")

(defvar *parked* (make-hash-table :test #'equal)
  "Account label -> the universal time its park ends, 0 for until unparked.")

;;; --- what the pool reads ----------------------------------------------------------

(defparameter +providers+
  '(("anthropic" "Claude") ("openai-codex" "Codex"))
  "Provider id -> the word the report uses for it. The two the original reads.")

(defparameter +anthropic-usage+ "https://api.anthropic.com/api/oauth/usage")
(defparameter +anthropic-messages+ "https://api.anthropic.com/v1/messages")
(defparameter +codex-usage+ "https://chatgpt.com/backend-api/wham/usage")
(defparameter +codex-responses+ "https://chatgpt.com/backend-api/codex/responses")

(defparameter +claude-code-agent+ "claude-code/2.1.0"
  "The client the Claude usage endpoint answers for: it rate-limits a bare
urllib user agent far sooner than a Claude Code one, same token and second.")

(defparameter +claude-code-system+ "You are Claude Code, Anthropic's official CLI for Claude."
  "The system line a wake sends, so the request counts as one Claude Code turn.")

(defparameter +codex-agent+ "codex_cli_rs/0.0.0"
  "The client the Codex usage endpoint answers for.")

(defun provider-label (id)
  "The word a report uses for provider ID."
  (or (second (assoc id +providers+ :test #'equal)) (princ-to-string id)))

;;; --- the accounts -----------------------------------------------------------------

(defun trim (value)
  "VALUE as a trimmed string, or NIL when it is not a non-empty string."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun parse-account (row)
  "ROW as a plist (:LABEL :PROVIDER :ENV), or NIL when it is not one.
ROW is \"label:provider:ENV_VAR\", or \"label:ENV_VAR\" for a Claude one."
  (let* ((row (trim row))
         (parts (and row (mapcar #'trim (uiop:split-string row :separator '(#\:)))))
         (parts (and parts (every #'identity parts) parts)))
    (cond
      ((null parts) nil)
      ((= 2 (length parts))
       (destructuring-bind (label env) parts
         (list :label label :provider "anthropic" :env env)))
      ((= 3 (length parts))
       (destructuring-bind (label provider env) parts
         (let ((provider (string-downcase provider)))
           (when (assoc provider +providers+ :test #'equal)
             (list :label label :provider provider :env env)))))
      (t nil))))

(defun getf-values (key)
  "The section's KEY member as a list of rows, in the order written."
  (let ((value (setting key)))
    (cond ((vectorp value) (coerce value 'list))
          ((listp value) value)
          (t nil))))

(defun accounts ()
  "Every account the section lists, in the order it lists them."
  (let ((rows (getf-values :accounts)))
    (remove nil (mapcar #'parse-account rows))))

(defun account-labels ()
  "Every account's label, in the order the section lists them."
  (running-settings)
  (mapcar (lambda (account) (getf account :label)) (accounts)))

(defun account-entry (label)
  "The account LABEL names, or a refusal listing the ones there are."
  (let* ((label (trim (and label (princ-to-string label))))
         (account (and label (find label (accounts) :key (lambda (row) (getf row :label)) :test #'equal))))
    (or account
        (fail "~@[~a is not an account this pool holds: ~]the pool holds ~{~a~^, ~}"
              label (account-labels)))))

(defun token (account)
  "ACCOUNT's access token, read from the environment variable it names, or NIL."
  (let ((value (uiop:getenv (getf account :env))))
    (and (stringp value) (trim value))))

;;; --- the shapes ---------------------------------------------------------------
;;; A snapshot is a plist: :AVAILABLE, :LABEL, :PROVIDER, :WINDOWS, :DETAILS,
;;; :REASON, :SOURCE. A window is a plist: :LABEL, :USED (percent, or NIL
;;; when the plan says unlimited), :REMAINING, :RESET, :DETAIL.

(defun unavailable (reason &key (source ""))
  "A snapshot for an account that answered a reason instead of numbers."
  (list :available nil :label nil :provider nil :windows nil :details nil
        :reason reason :source source))

(defun snapshot (&key windows details (source ""))
  (list :available t :label nil :provider nil :windows windows :details details
        :reason nil :source source))

(defun now-iso ()
  "The time, as the UTC ISO-8601 string the answer opens with."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun reset-text (value)
  "VALUE as a readable reset time: an ISO string passes through, a unix
epoch reads as UTC, anything else is left out."
  (cond ((null value) nil)
        ((stringp value) (trim value))
        ((realp value)
         (let ((universal (and (> value 100000000) (- (truncate value) 2208988800))))
           (and universal (plusp universal) (iso-of universal))))
        (t nil)))

(defun iso-of (universal)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0dZ" year month day hour minute)))

(defun window (label used &key reset detail)
  "One usage window: USED is a percentage, clamped to 0..100, or NIL for unlimited."
  (let ((used (and used (max 0.0 (min 100.0 (coerce used 'single-float))))))
    (list :label (or label "Window") :used used
          :remaining (and used (max 0.0 (- 100.0 used)))
          :reset (reset-text reset) :detail detail)))

(defun percent-text (window)
  "One window's line: the share used, clamped, and what is left."
  (let ((used (getf window :used)))
    (if used
        (format nil "~d% used, ~d% left" (round used) (round (getf window :remaining)))
        "unlimited")))

(defun worst-used (snapshot)
  "SNAPSHOT's most constraining window as a percentage, or NIL when it read none."
  (let ((used (remove nil (mapcar (lambda (window) (getf window :used))
                                  (getf snapshot :windows)))))
    (and used (reduce #'max used))))

;;; --- reading JSON ---------------------------------------------------------------

(defun number-string (text)
  "TEXT as a number when it is one, else NIL."
  (when (and (stringp text) (plusp (length text))
             (every (lambda (c) (or (digit-char-p c) (find c "-+.eE"))) text))
    (ignore-errors (read-from-string text))))

(defun num (object &rest path)
  "A number at PATH in OBJECT, or NIL: a string that names one reads as one."
  (let ((value (apply #'nlk:json-value object :any path)))
    (cond ((realp value) value)
          ((stringp value) (number-string value))
          (t nil))))

(defun get-json (url &key headers)
  "GET URL => (values OBJECT STATUS). OBJECT is the decoded body, or NIL
when the body was not JSON; a dial that failed is a condition for the caller."
  (multiple-value-bind (body status)
      (funcall *http* :get url :headers headers :timeout (setting :timeout-seconds))
    (values (and (stringp body) (plusp (length body))
                 (ignore-errors (nlk:decode-json body)))
            status)))

(defun refused-snapshot (status label source)
  "A snapshot for an HTTP status that is not a number to report."
  (cond ((member status '(401 403))
         (unavailable (format nil "~a rejected the token (HTTP ~a): sign in again, or another account's token" label status)
                      :source source))
        ((and (numberp status) (>= status 400))
         (unavailable (format nil "~a answered HTTP ~a" label status) :source source))
        (t nil)))

;;; --- the two providers ------------------------------------------------------------

(defun anthropic-headers (token)
  (list (cons "authorization" (format nil "Bearer ~a" token))
        (cons "accept" "application/json")
        (cons "content-type" "application/json")
        (cons "anthropic-beta" "oauth-2025-04-20")
        (cons "user-agent" +claude-code-agent+)))

(defun probe-anthropic (account)
  "ACCOUNT's Claude usage windows, read from Anthropic's own OAuth endpoint."
  (let ((token (token account)))
    (if (null token)
        (unavailable (format nil "~a has no token: export ~a, the variable it names"
                             (getf account :label) (getf account :env))
                     :source "anthropic_usage")
        (multiple-value-bind (payload status)
            (get-json +anthropic-usage+ :headers (anthropic-headers token))
          (or (refused-snapshot status "Claude" "anthropic_usage")
              (and payload (anthropic-snapshot payload))
              (unavailable (format nil "Claude answered no JSON (HTTP ~a)" status)
                           :source "anthropic_usage"))))))

(defun anthropic-snapshot (payload)
  (let ((windows '()))
    (dolist (row '(("five_hour" "5-hour") ("seven_day" "Weekly") ("seven_day_opus" "Weekly (Opus)")))
      (let* ((entry (nlk:json-value payload :object (first row)))
             (used (num entry "utilization")))
        (when used
          (push (window (second row) used
                        :reset (nlk:json-value entry :string "resets_at"))
                windows))))
    (if windows
        (snapshot :windows (nreverse windows) :source "anthropic_usage")
        (unavailable "Claude answered no usage window for this account"
                     :source "anthropic_usage"))))

(defun base64-decode (text)
  "Decode base64 or base64url TEXT into the string of the bytes it holds, or NIL."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (bits 0) (count 0) (bytes '()))
    (loop for c across (or text "")
          for value = (position (case c (#\- #\+) (#\_ #\/) (t c)) alphabet)
          when value
            do (setf bits (+ (* bits 64) value) count (+ count 6))
               (when (>= count 8)
                 (setf count (- count 8))
                 (push (ldb (byte 8 count) bits) bytes)
                 (setf bits (ldb (byte count 0) bits))))
    (when bytes (map 'string #'code-char (nreverse bytes)))))

(defun jwt-claims (token)
  "The payload of TOKEN as data, unverified: only an account id and an email
are read out of it, never a signature."
  (let* ((parts (and (stringp token) (uiop:split-string token :separator '(#\.))))
         (text (and (= 3 (length parts)) (base64-decode (second parts)))))
    (and text (ignore-errors (nlk:decode-json text)))))

(defun chatgpt-account-id (token)
  "The ChatGPT workspace the token belongs to, when the token names one."
  (let ((auth (nlk:json-value (jwt-claims token) :object "https://api.openai.com/auth")))
    (nlk:json-value auth :string "chatgpt_account_id")))

(defun codex-headers (token)
  (let ((headers (list (cons "authorization" (format nil "Bearer ~a" token))
                        (cons "accept" "application/json")
                        (cons "user-agent" +codex-agent+)
                        (cons "originator" +codex-agent+))))
    (let ((account (chatgpt-account-id token)))
      (if account (cons (cons "chatgpt-account-id" account) headers) headers))))

(defun probe-codex (account)
  "ACCOUNT's Codex usage windows, read from the endpoint Codex CLI's /status reads."
  (let ((token (token account)))
    (if (null token)
        (unavailable (format nil "~a has no token: export ~a, the variable it names"
                             (getf account :label) (getf account :env))
                     :source "codex_usage")
        (multiple-value-bind (payload status)
            (get-json +codex-usage+ :headers (codex-headers token))
          (or (refused-snapshot status "Codex" "codex_usage")
              (and payload (codex-snapshot payload))
              (unavailable (format nil "Codex answered no JSON (HTTP ~a)" status)
                           :source "codex_usage"))))))

(defun codex-snapshot (payload)
  (let* ((limits (nlk:json-value payload :object "rate_limit"))
         (reached (nlk:json-value limits :boolean "limit_reached"))
         (windows '()))
    (dolist (row '(("primary_window" "5-hour") ("secondary_window" "Weekly")))
      (let* ((entry (nlk:json-value limits :object (first row)))
             (used (num entry "used_percent"))
             (seconds (num entry "limit_window_seconds"))
             (after (num entry "reset_after_seconds")))
        (when used
          (push (window (if (and (equal (first row) "primary_window")
                                            seconds (plusp seconds) (/= seconds 18000))
                            (format nil "~dh window" (round seconds 3600))
                            (second row))
                        used
                        :reset (or (num entry "reset_at")
                                   (and after (+ (get-universal-time) (truncate after)))))
                windows))))
    (let ((windows (nreverse windows)))
      (cond ((null windows)
             (unavailable "Codex answered no usage window for this account"
                          :source "codex_usage"))
            (t
             (when reached
               ;; A reached limit is a full window, whatever the percentage said.
               (dolist (window windows)
                 (setf (getf window :used) 100.0 (getf window :remaining) 0.0)))
             (snapshot :windows windows
                       :details (and reached (list "the limit is reached: nothing more until it resets"))
                       :source "codex_usage"))))))

(defun probe (account)
  "ACCOUNT's snapshot, from its provider's own endpoint."
  (if (equal (getf account :provider) "openai-codex")
      (probe-codex account)
      (probe-anthropic account)))

(defun snapshot-for (account &key refresh)
  "ACCOUNT's snapshot: from the cache while it is fresh, else a fresh read.
REFRESH asks the vendor again."
  (let* ((label (getf account :label))
         (ttl (setting :cache-seconds))
         (hit (gethash label *cache*)))
    (if (and hit (not refresh) (<= (- (get-universal-time) (car hit)) ttl))
        (cdr hit)
        (let ((snapshot (probe account)))
          (setf (getf snapshot :label) label)
          (setf (getf snapshot :provider) (getf account :provider))
          (setf (gethash label *cache*) (cons (get-universal-time) snapshot))
          snapshot))))

;;; --- parks ----------------------------------------------------------------------

(defun state-path ()
  "Where the park decisions are kept, or NIL when they are kept in memory only."
  (let ((name (setting :state-file)))
    (cond ((pathnamep name) name)
          ((trim name) (nlk:home (trim name)))
          (t nil))))

(defun parked-p (label)
  "Whether LABEL is parked now."
  (let ((until (and label (gethash label *parked*))))
    (and until (or (zerop until) (> until (get-universal-time))) t)))

(defun load-parks ()
  "Read the park decisions back, dropping the ones whose time has passed."
  (clrhash *parked*)
  (let ((path (state-path)))
    (when (and path (probe-file path))
      (with-open-file (in path :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              for tab = (position #\Tab line)
              do (when tab
                   (let ((label (subseq line 0 tab))
                         (until (parse-integer (subseq line (1+ tab)) :junk-allowed t)))
                     (when (and label until
                                (or (zerop until) (> until (get-universal-time))))
                       (setf (gethash label *parked*) until)))))))))

(defun save-parks ()
  "Write the park decisions down, newest state of every parked account."
  (let ((path (state-path)))
    (when path
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                 :external-format :utf-8)
        (maphash (lambda (label until)
                   (when (or (zerop until) (> until (get-universal-time)))
                     (format out "~a~c~d~%" label #\Tab until)))
                 *parked*)))))

(define-verb park (label &key hours)
  "Keep an account out of the rotation -- out of best, and marked parked in
  the report -- until (account-pool:unpark LABEL), or for HOURS when given.
  Nothing is sent to the vendor: parking is this machine's own decision."
  (let* ((account (account-entry label))
         (label (getf account :label))
         (hours (and (realp hours) (plusp hours) (round hours)))
         (until (if hours (+ (get-universal-time) (* 3600 hours)) 0)))
    (setf (gethash label *parked*) until)
    (save-parks)
    (remhash label *cache*)
    (format nil "~a is parked~@[ for ~d hour~:p~]: best leaves it out until it is unparked."
            label hours)))

(define-verb unpark (label)
  "Put a parked account back in the rotation."
  (let* ((account (account-entry label))
         (label (getf account :label)))
    (remhash label *parked*)
    (save-parks)
    (remhash label *cache*)
    (format nil "~a is back in the rotation." label)))

;;; --- the answer ------------------------------------------------------------------

(defun targets (account)
  "The accounts a report reads: ACCOUNT when named, else every one the section lists."
  (let ((account (trim (and account (princ-to-string account)))))
    (if account (list (account-entry account)) (accounts))))

(define-verb windows (account &key refresh)
  "ACCOUNT's snapshot as a plist: :AVAILABLE, :WINDOWS (each a plist of
  :LABEL, :USED, :REMAINING, :RESET and :DETAIL), :DETAILS, :REASON and
  :SOURCE. REFRESH asks the vendor again instead of answering from the cache."
  (snapshot-for (account-entry account) :refresh refresh))

(defun print-snapshot (out snapshot)
  (let* ((label (getf snapshot :label))
         (used (worst-used snapshot)))
    (format out "~a (~a)~@[ - ~d% used, ~d% left~] - ~a~%"
            label (provider-label (getf snapshot :provider))
            (and used (round used)) (and used (round (- 100.0 used)))
            (if (parked-p label) "parked" "in rotation"))
    (when (getf snapshot :reason)
      (format out "  ~a~%" (getf snapshot :reason)))
    (dolist (window (getf snapshot :windows))
      (format out "  ~a: ~a~@[; resets ~a~]~@[ - ~a~]~%"
              (getf window :label) (percent-text window)
              (getf window :reset) (getf window :detail)))
    (dolist (detail (getf snapshot :details))
      (format out "  ~a~%" detail))))

(define-verb report (&key account refresh)
  "The whole pool as text: one block per account, every usage window with
  what is left and when it resets, and whether the account is in rotation or
  parked. ACCOUNT names one; REFRESH asks the vendors again instead of
  answering from the cache."
  (let* ((accounts (targets account))
         (started (get-internal-real-time))
         (snapshots (mapcar (lambda (row) (snapshot-for row :refresh refresh)) accounts)))
    (with-output-to-string (out)
      (format out "Account pool ~a~%" (now-iso))
      (dolist (snapshot snapshots) (print-snapshot out snapshot))
      (format out "~d account~:p, ~d parked, ~,2fs, ~a.~%"
              (length snapshots)
              (count-if #'parked-p (mapcar (lambda (row) (getf row :label)) accounts))
              (/ (- (get-internal-real-time) started)
                 (coerce internal-time-units-per-second 'single-float))
              (if refresh "asked just now"
                  (format nil "reused for up to ~ds" (setting :cache-seconds)))))))

(defun ranking ()
  "The accounts in the rotation, least used first. An account that is
parked, has no token or answered no window is left out."
  (let ((rows '()))
    (dolist (account (accounts))
      (unless (parked-p (getf account :label))
        (let* ((snapshot (snapshot-for account))
               (used (and (getf snapshot :available) (worst-used snapshot))))
          (when used
            (push (list :label (getf account :label) :used used :snapshot snapshot) rows)))))
    (sort rows #'< :key (lambda (row) (getf row :used)))))

(define-verb best ()
  "Which account to send the next request to: the one in the rotation with
  the most headroom, and the ones behind it. Reads the cache, not the vendor."
  (let ((rows (ranking)))
    (with-output-to-string (out)
      (if (null rows)
          (format out "No account in the pool is usable now: every one is parked, has no token, or answered no usage window.~%")
          (let ((top (first rows)))
            (format out "Use ~a: ~d% used, ~d% left.~%"
                    (getf top :label) (round (getf top :used))
                    (round (- 100.0 (getf top :used))))
            (dolist (row (rest rows))
              (format out "  next: ~a at ~d% used~%" (getf row :label) (round (getf row :used))))))
      (format out "~d account~:p in the rotation.~%" (length rows)))))

;;; --- waking an idle window ---------------------------------------------------------

(defun wake-answer (label vendor model status body)
  "The words one wake's answer earns."
  (cond ((and (numberp status) (< status 300))
         (remhash label *cache*)
         (format nil "woke ~a: one ~a request sent, so its 5-hour window starts now."
                 label model))
        ((eql status 429)
         (fail "~a is rate-limited: it is at a limit already, so its window is running." label))
        ((member status '(401 403))
         (fail "~a rejected the token (HTTP ~a): sign in again." label status))
        (t (fail "~a answered HTTP ~a~@[: ~a~]" vendor status
                 (and (stringp body) (plusp (length body))
                      (subseq body 0 (min 160 (length body))))))))

(defun wake-anthropic (account model)
  (let* ((label (getf account :label))
         (model (or (trim model) (setting :wake-model)))
         (payload (format nil "{\"model\":~s,\"max_tokens\":1,\"system\":[{\"type\":\"text\",\"text\":~s}],\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}"
                          model +claude-code-system+))
         (headers (list (cons "authorization" (format nil "Bearer ~a" (token account)))
                        (cons "content-type" "application/json")
                        (cons "accept" "application/json")
                        (cons "anthropic-version" "2023-06-01")
                        (cons "anthropic-beta" "oauth-2025-04-20")
                        (cons "user-agent" +claude-code-agent+))))
    (multiple-value-bind (body status)
        (funcall *http* :post +anthropic-messages+ :headers headers :content payload
                 :timeout (setting :wake-timeout-seconds))
      (wake-answer label "Claude" model status body))))

(defun wake-codex (account model)
  (let* ((label (getf account :label))
         (model (or (trim model) (trim (setting :codex-wake-model)) "gpt-5.4-mini"))
         (payload (format nil "{\"model\":~s,\"instructions\":\"Answer with one word.\",\"store\":false,\"stream\":true,\"input\":[{\"type\":\"message\",\"role\":\"user\",\"content\":[{\"type\":\"input_text\",\"text\":\"hi\"}]}],\"reasoning\":{\"effort\":\"low\"}}"
                          model))
         (headers (append (codex-headers (token account))
                          (list (cons "content-type" "application/json")
                                (cons "accept" "text/event-stream")))))
    (multiple-value-bind (body status)
        (funcall *http* :post +codex-responses+ :headers headers :content payload
                 :timeout (setting :wake-timeout-seconds))
      (cond ((and (stringp body) (search "response.failed" body))
             (fail "~a answered the request but it failed: ~a"
                   label (subseq body 0 (min 160 (length body)))))
            (t (wake-answer label "Codex" model status body))))))

(define-verb wake (account &key model)
  "Start an idle account's 5-hour window now: one tiny request through that
  account to its own vendor, with MODEL, or the section's wake model. Answers
  what was sent; a limit already running reads as rate-limited."
  (let* ((account (account-entry account))
         (token (token account)))
    (unless token
      (fail "~a has no token: export ~a, the variable it names"
            (getf account :label) (getf account :env)))
    (if (equal (getf account :provider) "openai-codex")
        (wake-codex account model)
        (wake-anthropic account model))))

;;; --- the words -----------------------------------------------------------------

(defparameter +primer+
  "The pool holds several Claude and Codex accounts, each with its own token in
the environment variable its entry names. Usage is read from the vendor that
owns each token (one GET), and nothing is shared between accounts.
  account_pool   the tool: {\"action\": report|best|park|unpark|wake,
      \"account\": label, \"refresh\": true|false}.
  (account-pool:report &key account refresh)   the same as text, through eval.
  (account-pool:best)          which account to use next: most headroom first.
  (account-pool:windows \"x\")  one account's windows as a plist.
  (account-pool:park \"x\")     keep an account out of the rotation (/account-pool unpark).
  (account-pool:wake \"x\")     one tiny request, so an idle 5-hour window starts.
/account-pool prints the report; /account-pool best, park LABEL, unpark LABEL,
wake LABEL, refresh do the rest. An answer is reused for cache_seconds;
refresh asks the vendor again. A refusal is ERROR: ACCOUNT-POOL-ERROR and
says what to set."
  "What (help :account-pool) answers while the cell runs.")

;;; --- the boundary ---------------------------------------------------------------

(nlk:define-contract pool-call
  (:doc "The operator's pool of Claude and Codex accounts: each account's usage windows, which one to send the next request to, and the switch that keeps an account out of the rotation. One GET per account to the vendor that owns its token; a wake is one tiny request to that same vendor.")
  ("action" :text :default "report" :options '("report" "best" "park" "unpark" "wake")
            :doc "report: every account's windows; best: the one with the most headroom; park or unpark: keep a label out of the rotation or put it back; wake: start an idle account's 5-hour window now")
  ("account" :text
             :doc "the account's label as the accounts list spells it; park, unpark and wake need it, report takes it to read one account")
  ("refresh" :boolean
             :doc "ask the vendors again instead of answering from the cache (cache_seconds); use it when the numbers moved"))

(defun guarded (body)
  "Run BODY, answering its text; a refusal or a dial that failed becomes the
  tool answer ERROR: ..., so the model reads what went wrong."
  (handler-case (funcall body)
    (account-pool-error (condition) (nle:failure (format nil "ERROR: ACCOUNT-POOL-ERROR: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: ACCOUNT-POOL-ERROR: ~a" condition)))))

(defun pool-tool (call)
  "The account_pool tool: the contract's call, answered as its text."
  (guarded
   (lambda ()
     (let ((action (string-downcase (or (trim (pool-call-action call)) "report")))
           (account (pool-call-account call)))
       (cond ((equal action "best") (best))
             ((equal action "park") (park account))
             ((equal action "unpark") (unpark account))
             ((equal action "wake") (wake account))
             (t (report :account account :refresh (pool-call-refresh call))))))))

(defun run-command (args session-id)
  "The /account-pool command: no argument is the report, 'refresh' asks the
  vendors again, 'best' picks, 'park', 'unpark' and 'wake' take a label, and
  any other word is one account's report."
  (declare (ignore session-id))
  (let* ((words (and args (remove "" (uiop:split-string (nlk:trimmed args)
                                                        :separator '(#\Space #\Tab)))))
         (word (and words (string-downcase (first words)))))
    (guarded
     (lambda ()
       (cond ((null words) (report))
             ((member word '("refresh" "again") :test #'equal) (report :refresh t))
             ((equal word "best") (best))
             ((equal word "park") (park (second words)))
             ((equal word "unpark") (unpark (second words)))
             ((equal word "wake") (wake (second words)))
             ((member word '("report" "list" "usage") :test #'equal)
              (report :account (second words)))
             (t (report :account (first words))))))))

(defun read-section (values table)
  "The section's values, every account entry parsed and at least one token set."
  (declare (ignore table))
  (let ((rows (let ((value (getf values :accounts)))
                (cond ((vectorp value) (coerce value 'list))
                      ((listp value) value)
                      (t nil))))
        (names '()))
    (unless rows
      (nlk:config-error "account-pool: the accounts list is empty. Name one account per entry, as label:provider:ENV_VAR, with the provider anthropic or openai-codex"))
    (dolist (row rows)
      (let ((account (parse-account row)))
        (unless account
          (nlk:config-error "account-pool: ~s is not an account entry. Write label:provider:ENV_VAR, with the provider anthropic or openai-codex, or label:ENV_VAR for a Claude one" row))
        (push (getf account :env) names)))
    (unless (some (lambda (name)
                    (let ((value (uiop:getenv name)))
                      (and value (trim value))))
                  names)
      (nlk:config-error "account-pool: no account's token is set. Export one of ~{~a~^, ~}, the variable that entry names" (nreverse names)))
    values))

(defun start ()
  (clrhash *cache*)
  (load-parks)
  (nle:on-stop (lambda ()
                 (clrhash *cache*)
                 (clrhash *parked*))))

(nle:define-cell account-pool
  (:section ("account-pool")
    (:guide "one entry per account in accounts, as label:provider:ENV_VAR, the provider anthropic or openai-codex (or label:ENV_VAR for a Claude one); each entry's token is read from the environment variable it names, so an account whose variable is unset reads back as one to sign in; with none set the cell refuses at start naming them; /account-pool then prints the pool, best picks one, park keeps one out of the rotation and wake starts an idle window")
    ("accounts" :list
                :doc "one account per entry: label:provider:ENV_VAR, provider anthropic or openai-codex; or label:ENV_VAR for a Claude account")
    ("cache_seconds" :integer :default 60 :min 0
                     :doc "how long one account's usage answer is reused before it is asked again")
    ("timeout_seconds" :integer :default 12 :min 1
                       :doc "the ceiling on one usage read")
    ("wake_timeout_seconds" :integer :default 60 :min 1
                            :doc "the ceiling on one wake request")
    ("wake_model" :string :default "claude-haiku-4-5"
                  :doc "the Claude model a wake sends to")
    ("codex_wake_model" :string :default "gpt-5.4-mini"
                        :doc "the Codex model a wake sends to")
    ("state_file" :path :default "account-pool/parked.txt"
                  :doc "where park decisions are kept, beside the home; empty keeps them in memory only"))
  (:settings #'read-section)
  (:tool "account_pool" (nlk:contract 'pool-call) 'pool-tool)
  (:command "account-pool" 'run-command
            :description "Several Claude and Codex accounts: usage, best, park, wake"
            :argument-hint "[report|best|park LABEL|unpark LABEL|wake LABEL|refresh]")
  (:help :account-pool
         "account-pool: several Claude and Codex accounts - the account_pool tool, (account-pool:report), /account-pool"
         +primer+)
  (:start #'start))
