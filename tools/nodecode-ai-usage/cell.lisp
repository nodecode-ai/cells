;;;; cell.lisp --- how much of each AI subscription is left, read from the plan itself.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Read-only. Seven probes, each one GET to the provider that owns the key
;;;; it uses -- GitHub Copilot, OpenCode Go, OpenCode Zen, Z.AI, Kimi,
;;;; MiniMax and DeepSeek -- and nothing sent anywhere else. One tool
;;;; (ai_usage), one command (/quotas), one manual topic, and two Lisp
;;;; functions for eval: (ai-usage:report ...) and (ai-usage:windows ...).
;;;;
;;;; The original is a Hermes desktop plugin: a page, a status-bar chip and a
;;;; FastAPI backend that resolved keys from Hermes's own credential store,
;;;; one profile at a time, and delegated four providers to Hermes's account
;;;; helper. Here the keys are environment variables the section names, and
;;;; the page is the report text.

(defpackage #:nodecode-ai-usage
  (:use #:cl)
  (:nicknames #:ai-usage)
  (:export #:report #:windows #:credential #:provider-ids #:ai-usage-error))

(in-package #:nodecode-ai-usage)

(nlk:define-peripheral ai-usage :not-running t)

(defvar *http* #'nlk:http
  "The one seam that leaves this image: an HTTP exchange. Tests stub it.")

(defvar *cache* (make-hash-table :test #'equal)
  "Provider id -> (UNIVERSAL-TIME . SNAPSHOT), while the cell runs.")

;;; --- what a probe reads -----------------------------------------------------------
;;; Data, not code: the ids are the original plugin's, so a caller that knew
;;; them there knows them here. Each row names the section member holding the
;;; variable its key lives in, that member's default, and the probe.

(defparameter +providers+
  '((:id "copilot" :label "GitHub Copilot" :env :copilot-token-env
     :default "COPILOT_GITHUB_TOKEN" :probe probe-copilot
     :reads "the plan's own quota windows: premium requests, chat, completions")
    (:id "zai" :label "Z.AI / GLM" :env :zai-key-env
     :default "GLM_API_KEY" :probe probe-zai
     :reads "the coding plan's session, weekly and tool quotas")
    (:id "kimi-coding" :label "Kimi / Kimi Coding Plan" :env :kimi-key-env
     :default "KIMI_API_KEY" :probe probe-kimi
     :reads "the coding plan's weekly window and any limit it lists")
    (:id "minimax" :label "MiniMax" :env :minimax-key-env
     :default "MINIMAX_API_KEY" :probe probe-minimax
     :reads "each model's 5-hour and weekly coding-plan windows")
    (:id "deepseek" :label "DeepSeek" :env :deepseek-key-env
     :default "DEEPSEEK_API_KEY" :probe probe-deepseek
     :reads "the account's prepaid balance")
    (:id "opencode-go" :label "OpenCode Go" :env :opencode-go-key-env
     :default "OPENCODE_GO_API_KEY" :probe probe-opencode-go
     :reads "the rolling, weekly and monthly windows")
    (:id "opencode-zen" :label "OpenCode Zen" :env :opencode-zen-key-env
     :default "OPENCODE_ZEN_API_KEY" :probe probe-opencode-zen
     :reads "the credit balance"))
  "Every provider this cell reads, in the order a report lists them.")

(defparameter +aliases+
  '(("glm" . "zai") ("moonshot" . "kimi-coding") ("openai" . "openai-api"))
  "The ids the original plugin normalised, kept so an old habit still resolves.")

(defun provider-ids ()
  "Every provider id this cell reads."
  (mapcar (lambda (entry) (getf entry :id)) +providers+))

(defun provider-entry (id)
  "The row for ID, after the alias table, or NIL."
  (let* ((id (string-downcase (nlk:trimmed (princ-to-string (or id ""))))))
    (find (or (cdr (assoc id +aliases+ :test #'equal)) id)
          +providers+ :key (lambda (entry) (getf entry :id)) :test #'equal)))

(defun credential-for (entry)
  "The key ENTRY's probe uses, from the variable the section names for it,
  or NIL when that variable is unset or empty."
  (let* ((name (setting (getf entry :env)))
         (name (and (stringp name) (nlk:trimmed name))))
    (when (and name (plusp (length name)))
      (let ((key (uiop:getenv name)))
        (and key (plusp (length (nlk:trimmed key))) (nlk:trimmed key))))))

(defun credential (provider)
  "The key the probe for PROVIDER uses, or NIL. A refusal names an id it does not read."
  (let ((entry (provider-entry provider)))
    (unless entry
      (fail "~s is not a provider this cell reads: ~{~a~^, ~}"
            provider (provider-ids)))
    (credential-for entry)))

;;; --- the shapes ----------------------------------------------------------------
;;; A snapshot is a plist: :AVAILABLE, :PROVIDER, :TITLE, :PLAN, :WINDOWS,
;;; :DETAILS, :REASON, :SOURCE. A window is a plist: :LABEL, :USED (percent,
;;; or NIL when the plan says unlimited), :REMAINING, :RESET, :DETAIL.

(defun unavailable (reason &key (source ""))
  "A snapshot for a provider that answered a reason instead of numbers."
  (list :available nil :provider nil :title "Account limits" :plan nil
        :windows nil :details nil :reason reason :source source))

(defun snapshot (id &key title plan windows details (source ""))
  (list :available t :provider id :title (or title "Account limits") :plan plan
        :windows windows :details details :reason nil :source source))

(defun window (label used &key reset detail)
  "One quota window: USED is a percentage, clamped to 0..100, or NIL for unlimited."
  (let ((used (and used (max 0.0 (min 100.0 (coerce used 'single-float))))))
    (list :label (or label "Window") :used used
          :remaining (and used (max 0.0 (- 100.0 used)))
          :reset (and reset (princ-to-string reset)) :detail detail)))

(defun now-iso ()
  "The time, as the UTC ISO-8601 string the original put on the wire."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun num (object &rest path)
  "A number at PATH, or NIL: a string that names one reads as one."
  (let ((value (apply #'nlk:json-value object :any path)))
    (cond ((realp value) value)
          ((stringp value) (ignore-errors (read-from-string value)))
          (t nil))))

(defun bearer (key)
  (list (cons "authorization" (format nil "Bearer ~a" key))
        (cons "accept" "application/json")))

(defun get-json (url &key headers)
  "GET URL => (values OBJECT STATUS). OBJECT is the decoded body, or NIL when
  the body was not JSON; a dial that failed is a condition for the caller."
  (multiple-value-bind (body status)
      (funcall *http* :get url :headers headers
               :timeout (setting :timeout-seconds))
    (values (and (stringp body) (plusp (length body))
                 (ignore-errors (nlk:decode-json body)))
            status)))

(defun refused (status label endpoint)
  "A snapshot for an HTTP status that is not a number to report."
  (cond ((member status '(401 403))
         (unavailable (format nil "~a rejected the key (HTTP ~a): it may be expired, or another workspace's"
                              label status) :source endpoint))
        ((and (numberp status) (>= status 400))
         (unavailable (format nil "~a answered HTTP ~a" label status) :source endpoint))
        (t nil)))

;;; --- the seven probes ------------------------------------------------------------

(defun probe-copilot ()
  (let ((entry (provider-entry "copilot"))
        (key (credential "copilot")))
    (if (null key)
        (unavailable (format nil "no GitHub Copilot token: export ~a, or name the variable that holds one"
                             (getf entry :default)) :source "copilot_internal")
        (multiple-value-bind (payload status)
            (funcall *http* :get "https://api.github.com/copilot_internal/user"
                     :headers (list (cons "authorization" (format nil "token ~a" key))
                                    (cons "accept" "application/json")
                                    (cons "editor-version" "vscode/1.96.2")
                                    (cons "editor-plugin-version" "copilot-chat/0.23.0")
                                    (cons "user-agent" "GitHubCopilotChat/0.23.0"))
                     :timeout (setting :timeout-seconds))
          (let ((payload (and (stringp payload) (plusp (length payload))
                              (ignore-errors (nlk:decode-json payload)))))
            (or (refused status "GitHub Copilot" "copilot_internal")
                (and payload (copilot-snapshot payload))
                (unavailable (format nil "GitHub Copilot answered no JSON (HTTP ~a)" status)
                             :source "copilot_internal")))))))

(defun copilot-snapshot (payload)
  (let ((snapshots (nlk:json-value payload :object "quota_snapshots"))
        (reset (or (nlk:json-value payload :string "quota_reset_date")
                   (nlk:json-value payload :string "limited_user_reset_date")))
        (plan (or (nlk:json-value payload :string "copilot_plan")
                  (nlk:json-value payload :string "access_type_sku")))
        (windows '()) (details '()))
    (dolist (row '(("premium_interactions" "Premium requests")
                   ("chat" "Chat") ("completions" "Completions")))
      (let* ((entry (nlk:json-value snapshots :object (first row)))
             (label (second row))
             (percent (num entry "percent_remaining"))
             (entitlement (num entry "entitlement"))
             (remaining (num entry "remaining")))
        (cond ((nlk:json-value entry :boolean "unlimited")
               (push (window label nil :reset reset :detail "unlimited") windows))
              ((and entitlement (<= entitlement 0))
               (push (format nil "~a: not included in this plan" label) details))
              ((null percent) nil)
              (t (push (window label (- 100.0 percent) :reset reset
                               :detail (and entitlement remaining
                                            (format nil "~d of ~d left" (round remaining) (round entitlement))))
                       windows)))))
    (let ((overage (num snapshots "premium_interactions" "overage_count")))
      (when (and overage (plusp overage))
        (push (format nil "Overage requests used: ~d" (round overage)) details)))
    (when plan (push (format nil "Plan: ~a" plan) details))
    (if (or windows details)
        (snapshot "copilot" :title "GitHub Copilot limits" :plan plan
                  :windows (nreverse windows) :details (nreverse details)
                  :source "copilot_internal")
        (unavailable "GitHub Copilot returned no quota windows for this account"
                     :source "copilot_internal"))))

(defun probe-opencode-go ()
  (let ((key (credential "opencode-go")))
    (if (null key)
        (unavailable "no OpenCode Go key: export OPENCODE_GO_API_KEY, or name the variable that holds one"
                     :source "zen_go_usage")
        (multiple-value-bind (payload status)
            (get-json "https://opencode.ai/zen/go/v1/usage" :headers (bearer key))
          (or (refused status "OpenCode Go" "zen_go_usage")
              (let* ((usage (nlk:json-value payload :object "usage"))
                     (windows '()))
                (dolist (row '(("rolling" "Rolling (5h)") ("weekly" "Weekly") ("monthly" "Monthly")))
                  (let* ((entry (nlk:json-value usage :object (first row)))
                         (percent (num entry "percent"))
                         (state (nlk:json-value entry :string "status")))
                    (when percent
                      (push (window (second row) percent
                                    :reset (or (nlk:json-value entry :string "resetsAt")
                                               (num entry "resetsAt"))
                                    :detail (and state (not (equal state "ok"))
                                                 (format nil "status: ~a" state)))
                            windows))))
                (if windows
                    (snapshot "opencode-go" :title "OpenCode Go limits" :plan "Go"
                              :windows (nreverse windows) :source "zen_go_usage")
                    (unavailable "OpenCode Go returned no usage windows" :source "zen_go_usage")))
              (unavailable (format nil "OpenCode Go answered no JSON (HTTP ~a)" status)
                           :source "zen_go_usage"))))))

(defun probe-opencode-zen ()
  (let ((key (credential "opencode-zen")))
    (if (null key)
        (unavailable "no OpenCode Zen key: export OPENCODE_ZEN_API_KEY, or name the variable that holds one"
                     :source "zen_credits")
        (let ((answer nil))
          (dolist (url '("https://api.opencode.ai/v1/credits" "https://opencode.ai/zen/v1/credits"))
            (unless answer
              (multiple-value-bind (payload status)
                  (get-json url :headers (bearer key))
                (setf answer
                      (or (refused status "OpenCode Zen" "zen_credits")
                          (and payload (zen-credits-snapshot payload)))))))
          (or answer
              (unavailable (format nil "OpenCode Zen exposes no credits endpoint for this key -- it is pay-as-you-go")
                           :source "zen_credits"))))))

(defun zen-credits-snapshot (payload)
  (let* ((data (or (nlk:json-value payload :object "data") payload))
         (total (num data "total_credits"))
         (used (num data "used_credits"))
         (remaining (num data "remaining_credits")))
    (cond ((and total used (plusp total))
           (snapshot "opencode-zen" :title "OpenCode Zen credits"
                     :windows (list (window "Credits" (/ (* 100.0 used) total)
                                            :detail (format nil "$~,2f of $~,2f left"
                                                            (or remaining 0) total)))
                     :source "zen_credits"))
          (remaining
           (snapshot "opencode-zen" :title "OpenCode Zen credits"
                     :details (list (format nil "Credits remaining: $~,2f" remaining))
                     :source "zen_credits"))
          (t nil))))

(defun probe-zai ()
  (let ((key (credential "zai")))
    (if (null key)
        (unavailable "no Z.AI / GLM key: export GLM_API_KEY, or name the variable that holds one"
                     :source "zai_quota")
        (multiple-value-bind (payload status)
            (get-json "https://api.z.ai/api/monitor/usage/quota/limit" :headers (bearer key))
          (or (refused status "Z.AI" "zai_quota")
              (let ((windows '()))
                (dolist (item (coerce (nlk:json-value payload :array "limits") 'list))
                  (when (hash-table-p item)
                    (let* ((type (nlk:json-value item :string "type"))
                           (percent (num item "percentage"))
                           (total (num item "total"))
                           (current (num item "currentValue"))
                           (usage (num item "usage"))
                           (base (or total usage))
                           (label (cond ((equal type "TOKENS_LIMIT") "Session (5h)")
                                        ((equal type "TIME_LIMIT") "Tool quota")
                                        ((eql (num item "unit") 6) "Weekly")
                                        (t (string-capitalize (substitute #\Space #\_ (string-downcase (or type "limit"))))))))
                      (when (and (null percent) current base (plusp base))
                        (setf percent (/ (* 100.0 current) base)))
                      (when percent
                        (push (window label percent
                                      :detail (let ((remaining (num item "remaining")))
                                                (and remaining (format nil "~a left" remaining))))
                              windows)))))
                (if windows
                    (snapshot "zai" :title "Z.AI coding plan limits"
                              :windows (nreverse windows) :source "zai_quota")
                    (unavailable "Z.AI returned no parseable quota limits" :source "zai_quota")))
              (unavailable (format nil "Z.AI answered no JSON (HTTP ~a)" status) :source "zai_quota"))))))

(defun probe-kimi ()
  (let ((key (credential "kimi-coding")))
    (if (null key)
        (unavailable "no Kimi key: export KIMI_API_KEY, or name the variable that holds one"
                     :source "kimi_usages")
        (multiple-value-bind (payload status)
            (get-json "https://api.kimi.com/coding/v1/usages" :headers (bearer key))
          (or (refused status "Kimi" "kimi_usages")
              (let* ((usage (nlk:json-value payload :object "usage"))
                     (used (num usage "used"))
                     (limit (num usage "limit"))
                     (windows '()))
                (when (and used limit (plusp limit))
                  (push (window "Weekly" (/ (* 100.0 used) limit)
                                :reset (nlk:json-value usage :string "resetTime")
                                :detail (format nil "~d of ~d left" (round (max 0.0 (- limit used))) (round limit)))
                        windows))
                (dolist (entry (coerce (nlk:json-value payload :array "limits") 'list))
                  (let ((percent (num entry "percentage")))
                    (when percent
                      (push (window (or (nlk:json-value entry :string "window")
                                        (nlk:json-value entry :string "name") "Limit")
                                    percent
                                    :reset (or (nlk:json-value entry :string "resetTime")
                                               (nlk:json-value entry :string "reset_time")))
                            windows))))
                (if windows
                    (snapshot "kimi-coding" :title "Kimi coding plan limits"
                              :windows (nreverse windows) :source "kimi_usages")
                    (unavailable "Kimi returned no parseable usage windows" :source "kimi_usages")))
              (unavailable (format nil "Kimi answered no JSON (HTTP ~a)" status) :source "kimi_usages"))))))

(defun probe-minimax ()
  (let ((key (credential "minimax")))
    (if (null key)
        (unavailable "no MiniMax key: export MINIMAX_API_KEY, or name the variable that holds one"
                     :source "minimax_remains")
        (let ((answer nil) (last-status nil))
          (dolist (url '("https://api.minimax.io/v1/api/openplatform/coding_plan/remains"
                         "https://www.minimax.io/v1/api/openplatform/coding_plan/remains"))
            (unless answer
              (multiple-value-bind (payload status)
                  (get-json url :headers (bearer key))
                (setf last-status status)
                (setf answer (or (and (member status '(401 403))
                                      (refused status "MiniMax" "minimax_remains"))
                                 (and payload (minimax-snapshot payload)))))))
          (or answer
              (unavailable (format nil "MiniMax answered no parseable coding-plan rows (HTTP ~a)" last-status)
                           :source "minimax_remains"))))))

(defun minimax-snapshot (payload)
  (let ((windows '()))
    (dolist (row (coerce (nlk:json-value payload :array "model_remains") 'list))
      (when (hash-table-p row)
        (let ((name (or (nlk:json-value row :string "model_name") "Model")))
          (dolist (pair '(("current_interval_total_count" "current_interval_usage_count" "5h")
                          ("current_weekly_total_count" "current_weekly_usage_count" "weekly")))
            (let ((total (num row (first pair)))
                  (left (num row (second pair))))
              (when (and total left (plusp total))
                (let ((left (min (max left 0) total)))
                  (push (window (format nil "~a . ~a" name (third pair))
                                (/ (* 100.0 (- total left)) total)
                                :detail (format nil "~d of ~d left" (round left) (round total)))
                        windows))))))))
    (if windows
        (snapshot "minimax" :title "MiniMax coding plan limits"
                  :windows (nreverse windows) :source "minimax_remains")
        (unavailable "MiniMax returned no parseable coding-plan rows" :source "minimax_remains"))))

(defun probe-deepseek ()
  (let ((key (credential "deepseek")))
    (if (null key)
        (unavailable "no DeepSeek key: export DEEPSEEK_API_KEY, or name the variable that holds one"
                     :source "deepseek_balance")
        (multiple-value-bind (payload status)
            (get-json "https://api.deepseek.com/user/balance" :headers (bearer key))
          (or (refused status "DeepSeek" "deepseek_balance")
              (let ((details '()))
                (dolist (info (coerce (nlk:json-value payload :array "balance_infos") 'list))
                  (let ((total (nlk:json-value info :string "total_balance"))
                        (currency (nlk:json-value info :string "currency")))
                    (when total
                      (push (format nil "~@[~a ~]~a balance" currency total) details))))
                (if details
                    (snapshot "deepseek" :title "DeepSeek balance"
                              :details (nreverse details) :source "deepseek_balance")
                    (unavailable "DeepSeek returned no balance rows" :source "deepseek_balance")))
              (unavailable (format nil "DeepSeek answered no JSON (HTTP ~a)" status)
                           :source "deepseek_balance"))))))

;;; --- one answer per provider, reused for cache_seconds --------------------------

(defun snapshot-for (entry &key refresh)
  "ENTRY's quota windows, from the cache while it is fresh, else a fresh probe."
  (let* ((id (getf entry :id))
         (ttl (setting :cache-seconds))
         (hit (gethash id *cache*)))
    (if (and hit (not refresh) (<= (- (get-universal-time) (car hit)) ttl))
        (cdr hit)
        (let ((snapshot (funcall (getf entry :probe))))
          (setf (getf snapshot :provider) id)
          (setf (gethash id *cache*) (cons (get-universal-time) snapshot))
          snapshot))))

(defun windows (provider &key refresh)
  "PROVIDER's snapshot as a plist: :AVAILABLE, :PLAN, :WINDOWS, :DETAILS,
  :REASON, :SOURCE. Each window is a plist of :LABEL, :USED, :REMAINING,
  :RESET and :DETAIL. A provider nothing is set for answers a :REASON."
  (running-settings)
  (snapshot-for (provider-entry provider) :refresh refresh))

(defun targets (provider)
  "The entries a report reads: PROVIDER when named, else every one with a key."
  (if provider
      (let ((entry (provider-entry provider)))
        (unless entry
          (fail "~s is not a provider this cell reads: ~{~a~^, ~}" provider (provider-ids)))
        (list entry))
      (remove-if-not #'credential-for +providers+)))

;;; --- the words -----------------------------------------------------------------

(defparameter +primer+
  "AI plan usage is available through the nodecode-ai-usage cell: how much of
each subscription is used, what is left and when it resets, read from the plan
itself (one GET to the provider that owns the key, nothing else).
  ai_usage   the tool: {\"provider\": id, \"refresh\": true|false}. Every
      provider whose key this home has, or one of them.
  (ai-usage:report &key provider refresh)   the same as text, through eval.
  (ai-usage:windows \"zai\")   one provider's windows as a plist: a list of
      (:label :used :remaining :reset :detail) under :windows.
Providers: copilot, zai (GLM), kimi-coding, minimax, deepseek, opencode-go,
opencode-zen. A key is read from the environment variable the ai-usage section
names for that provider; a provider with no key is left out of a full report,
and named one answers why. An answer is reused for cache_seconds; refresh asks
the plan again. /quotas shows the same text in the shell. A refusal is
ERROR: AI-USAGE-ERROR and says what to set."
  "What (help :ai-usage) answers while the cell runs.")

(defun percent-text (window)
  "One window's line: the share used, clamped, and what is left."
  (let ((used (getf window :used)))
    (if used
        (format nil "~d% used, ~d% left" (round used) (round (getf window :remaining)))
        "unlimited")))

(defun print-snapshot (out snapshot)
  (let* ((entry (provider-entry (getf snapshot :provider)))
         (label (or (getf entry :label) (getf snapshot :provider) "account")))
    (format out "~a (~a)~@[: ~a~]~@[ - plan ~a~]~%"
            (or (getf snapshot :provider) "?")
            label
            (getf snapshot :title)
            (getf snapshot :plan))
    (when (getf snapshot :reason)
      (format out "  ~a~%" (getf snapshot :reason)))
    (dolist (window (getf snapshot :windows))
      (format out "  ~a: ~a~@[; resets ~a~]~@[ - ~a~]~%"
              (getf window :label) (percent-text window)
              (getf window :reset) (getf window :detail)))
    (dolist (detail (getf snapshot :details))
      (format out "  ~a~%" detail))))

(define-verb report (&key provider refresh)
  "How much of each AI subscription is left: one block per provider, with
  every quota window it reports, what is left and when it resets. PROVIDER
  names one id; REFRESH asks the plans again instead of answering from the
  cache. Answers the text the ai_usage tool and /quotas show."
  (let* ((entries (targets provider))
         (started (get-internal-real-time))
         (snapshots (mapcar (lambda (entry) (snapshot-for entry :refresh refresh)) entries)))
    (with-output-to-string (out)
      (format out "AI usage ~a~%" (now-iso))
      (if (null snapshots)
          (format out "No key is set for any provider this cell reads: export one of ~{~a~^, ~}~%"
                  (mapcar (lambda (entry) (getf entry :default)) +providers+))
          (dolist (snapshot snapshots) (print-snapshot out snapshot)))
      (format out "~d provider~:p, ~,2fs, ~a.~%"
              (length snapshots)
              (/ (- (get-internal-real-time) started)
                 (coerce internal-time-units-per-second 'single-float))
              (if refresh "asked just now" (format nil "reused for up to ~ds" (setting :cache-seconds)))))))

;;; --- the boundary ---------------------------------------------------------------

(nlk:define-contract usage-call
  (:doc "How much of the operator's AI subscriptions is left. One block per provider whose key this home has: every quota window the plan reports, the percentage used and left, when it resets, and the plan's own details -- the balance for a pay-as-you-go key. Reads GitHub Copilot, Z.AI / GLM, Kimi, MiniMax, DeepSeek, OpenCode Go and OpenCode Zen, one GET each to the provider itself.")
  ("provider" :text
              :doc "one provider id: copilot, zai, kimi-coding, minimax, deepseek, opencode-go or opencode-zen; leave it out for every provider with a key")
  ("refresh" :boolean
             :doc "ask the plans again instead of answering from the cache (cache_seconds); use it when the operator says the numbers moved"))

(defun guarded (body)
  "Run BODY, answering its text; a refusal or a dial that failed becomes the
  tool answer ERROR: ..., so the model reads what went wrong."
  (handler-case (funcall body)
    (ai-usage-error (condition) (nle:failure (format nil "ERROR: AI-USAGE-ERROR: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: AI-USAGE-ERROR: ~a" condition)))))

(defun usage-tool (call)
  "The ai_usage tool: the contract's call, answered as the report's text."
  (guarded (lambda ()
             (report :provider (usage-call-provider call)
                     :refresh (usage-call-refresh call)))))

(defun run-command (args session-id)
  "The /quotas command: no argument is the report, 'refresh' asks the plans
  again, any other word is one provider id."
  (declare (ignore session-id))
  (let ((words (and args (remove "" (uiop:split-string (nlk:trimmed args)
                                                        :separator '(#\Space #\Tab))))))
    (guarded (lambda ()
               (cond ((null words) (report))
                     ((member (string-downcase (first words)) '("refresh" "again") :test #'equal)
                      (report :refresh t))
                     (t (report :provider (first words))))))))

(defun read-section (values table)
  "The section's values, each *_env member defaulted to its variable's usual
  name, refusing when not one of those variables holds a key."
  (declare (ignore table))
  (let ((names '()))
    (dolist (entry +providers+)
      (let* ((member (getf entry :env))
             (name (nlk:trimmed (or (getf values member) "")))
             (name (if (plusp (length name)) name (getf entry :default))))
        (setf (getf values member) name)
        (push name names)))
    (unless (some (lambda (name)
                    (let ((key (uiop:getenv name)))
                      (and key (plusp (length (nlk:trimmed key))))))
                  names)
      (nlk:config-error "ai-usage: no provider key found. Export one of ~{~a~^, ~} with the key to that plan, or point that provider's *_env member at the variable that holds it" (nreverse names)))
    values))

(defun start ()
  (clrhash *cache*)
  (nle:on-stop (lambda () (clrhash *cache*))))

(nle:define-cell ai-usage
  (:section ("ai-usage")
    (:guide "a key is read from an environment variable: name the variable in that provider's *_env member (the defaults are the usual names -- GLM_API_KEY, COPILOT_GITHUB_TOKEN, KIMI_API_KEY, MINIMAX_API_KEY, DEEPSEEK_API_KEY, OPENCODE_GO_API_KEY, OPENCODE_ZEN_API_KEY); only providers whose variable is set are reported, and with none set the cell refuses at start naming them; /quotas then prints the report")
    ("copilot_token_env" :env :default "COPILOT_GITHUB_TOKEN"
                          :doc "the variable holding a GitHub token with Copilot access")
    ("zai_key_env" :env :default "GLM_API_KEY"
                    :doc "the variable holding a Z.AI / GLM coding-plan key")
    ("kimi_key_env" :env :default "KIMI_API_KEY"
                     :doc "the variable holding a Kimi coding-plan key")
    ("minimax_key_env" :env :default "MINIMAX_API_KEY"
                        :doc "the variable holding a MiniMax coding-plan key")
    ("deepseek_key_env" :env :default "DEEPSEEK_API_KEY"
                         :doc "the variable holding a DeepSeek key")
    ("opencode_go_key_env" :env :default "OPENCODE_GO_API_KEY"
                            :doc "the variable holding an OpenCode Go key")
    ("opencode_zen_key_env" :env :default "OPENCODE_ZEN_API_KEY"
                             :doc "the variable holding an OpenCode Zen key")
    ("cache_seconds" :integer :default 60 :min 0
                     :doc "how long one provider's answer is reused before it is asked again")
    ("timeout_seconds" :integer :default 12 :min 1
                       :doc "the ceiling on one probe"))
  (:settings #'read-section)
  (:tool "ai_usage" (nlk:contract 'usage-call) 'usage-tool)
  (:command "quotas" 'run-command
            :description "How much of each AI plan is left"
            :argument-hint "[provider|refresh]")
  (:help :ai-usage
         "ai-usage: how much of each AI subscription is left - the ai_usage tool, (ai-usage:report), /quotas"
         +primer+)
  (:start #'start))
