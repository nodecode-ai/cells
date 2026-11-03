;;;; wake.lisp --- turns nobody typed: the one wake job, its own clock, its gates.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A wake is a cron job of its own kind: the job "wake", one per store,
;;;; that the operator turns on with bounds -- the shortest and longest gap
;;;; between wakes, a dollar allowance per 24 hours, a tool-call limit --
;;;; and that then keeps its own clock. It is Ouroboros's background
;;;; consciousness (prompts/CONSCIOUSNESS.md, ouroboros/consciousness.py)
;;;; reduced to what cron already owns: the durable session, the
;;;; at-most-once fire, SILENT, the report to the session that scheduled it.
;;;;
;;;; The clock. A wake turn may pick its next wake with (cron:wake-next
;;;; "2h" "why"), held to the bounds: the one verb a job's own session may
;;;; call, and only on its own job. A wake that picks nothing doubles the
;;;; gap after a silent or failed wake, up to the longest, and resets it to
;;;; the shortest after one that spoke. Events never wake it: they wait as
;;;; news for the next due instant (Ouroboros decided the same, #1603).
;;;;
;;;; The gates run in Lisp before any model call, so a gate that holds
;;;; costs nothing. An allowance spent -- or a spend the store cannot price
;;;; while an allowance is set -- skips the fire. A due instant the wake did
;;;; not ask for, with no news since the last admitted wake and the longest
;;;; gap not yet run out, skips it too and doubles the gap. News is what
;;;; *WAKE-SOURCES* report since the cursor, which moves only when a fire
;;;; is admitted.
;;;;
;;;; The level. This version is observe only: in the wake's session every
;;;; tool call passes WAKE-CALL-REFUSAL first (the cell's :TOOL hook), which
;;;; refuses writes. It is a check inside this process, as every in-image
;;;; guard is: it catches a wake that drifts, it does not hold one that
;;;; rewrites the hook. What holds against that is outside the process: the
;;;; provider's prepaid balance, a token without admin rights.
;;;;
;;;; The stop. (cron:wake :off) pauses the job, records who stopped it, and
;;;; asks a wake in flight to stop. Only (cron:wake :on), from a session that
;;;; is not a job's, turns it on again: the generic resume, edit and run
;;;; refuse the wake job, and a job's session is refused every scheduling
;;;; verb.

(in-package #:nodecode-cron)

(defparameter +wake-id+ "wake"
  "The id, name and slug of the one wake job.")

(defparameter +wake-defaults+ (list :min (* 15 60) :max (* 4 3600) :calls 40)
  "Bounds a first (cron:wake :on) takes for every one it does not name.")

(defparameter *wake-prompt*
  "No one asked for this turn; it is yours. What changed since your last wake, what did you
promise and not yet do, what is worth understanding, checking or preparing? The run context
below names the news, your bounds and your spend: context, not a checklist.

This wake may only read: look, check, think. Its writes are refused, so when something should
be changed, say what and why. Do not manufacture activity. When nothing is worth a line,
answer exactly SILENT. When something is worth the operator's attention, or is a decision
only they can make, answer with it in a few lines: that answer goes to the session that
turned wakes on.

Before you end you may pick when to wake next: (cron:wake-next \"2h\" \"why\"), held to the
operator's bounds. Without it the gap doubles after a silent wake and resets after one that
spoke."
  "What every wake turn is asked. Data, so a layer can reword it.")

(defparameter *wake-sources* (list 'wake-source-cron-outcomes)
  "Functions of one argument, the instant of the last admitted wake, each
answering a list of one-line strings -- what it saw change since -- or
:UNAVAILABLE. Called under the cron lock at each due instant; one that
signals reads as unavailable. A file that adds a source pushes its symbol.")

(defvar *wake-jitter* (lambda (seconds) (random (max 1 seconds)))
  "Seconds added to the first wake, under (FUNCALL *WAKE-JITTER* SPAN): four
organisms turned on together do not wake together. A test binds it.")

;;; --- the record ---------------------------------------------------------------------

(defun wake-get (job key)
  (getf (job-wake job) key))

(defun wake-put (job &rest keys-and-values)
  "Set each KEY VALUE on JOB's wake plist; a NIL value drops the key."
  (loop for (key value) on keys-and-values by #'cddr
        do (if value
               (setf (getf (job-wake job) key) value)
               (remf (job-wake job) key))))

(defun wake-job ()
  "The wake job, or NIL. Caller holds the lock."
  (let ((job (find-job +wake-id+)))
    (and job (job-wake job) job)))

(defun wake-session-p (session)
  "Whether SESSION is the wake job's own session."
  (and (stringp session)
       (with-cron-lock
         (let ((job (wake-job)))
           (and job (string= session (job-session job)))))))

(defun wake-firing (job)
  (find (job-id job) *inflight* :key #'fire-job-id :test #'string=))

(defun wake-clamp (job seconds)
  (max (wake-get job :min) (min (wake-get job :max) seconds)))

(defun wake-safety-next (job now)
  "Where a fired wake's clock stands until its fire settles: its longest gap."
  (+ now (wake-get job :max)))

(defun usd (amount)
  (format nil "$~,2f" amount))

(defun seconds-from (value what)
  "VALUE -- \"15m\", \"2h\", \"in 90m\", \"1h 30m\" or whole seconds -- as seconds,
or a CRON-ERROR naming WHAT."
  (let ((seconds (cond ((and (integerp value) (plusp value)) value)
                       ((stringp value)
                        (let ((text (string-trim " " (string-downcase value))))
                          (nlk:parse-duration (if (uiop:string-prefix-p "in " text)
                                                  (subseq text 3)
                                                  text)))))))
    (unless (and (integerp seconds) (plusp seconds))
      (fail "~a: cannot read ~s as a duration (\"15m\", \"2h\", \"1h 30m\")" what value))
    seconds))

(defun wake-display (job)
  (format nil "wake, ~a-~a" (duration-text (wake-get job :min)) (duration-text (wake-get job :max))))

;;; --- news ---------------------------------------------------------------------------

(defun wake-source-cron-outcomes (since)
  "The other jobs whose newest outcome since SINCE failed, was cancelled,
missed its instant, overran or was not fired."
  (with-cron-lock
   (loop for job in *jobs*
        for last = (job-last job)
        when (and (null (job-wake job))
                  last
                  (> (getf last :at) since)
                  (member (getf last :status) '("failed" "cancelled" "missed" "overrun" "error")
                          :test #'equal))
          collect (format nil "cron ~a ~a at ~a~@[: ~a~]" (job-id job) (getf last :status)
                          (local-text (getf last :at)) (getf last :line)))))

(defun source-label (source)
  (string-downcase (if (symbolp source) (symbol-name source) "source")))

(defun wake-read-news (now &key any)
  "Outside the cron lock: when the wake job is due at NOW -- or on, under
ANY -- what its sources report since its cursor, as (SINCE NEWS GAPS); else
NIL. A source may call GitHub or scan the store, and the lock is what every
turn's :FRAME publication takes, so no source runs under it."
  (let ((since (with-cron-lock
                 (let ((job (wake-job)))
                   (and job (eq (job-state job) :active)
                        (or any (and (job-next-at job) (<= (job-next-at job) now)))
                        (or (wake-get job :cursor) (job-created-at job)))))))
    (when since
      (multiple-value-bind (news gaps) (wake-news since)
        (list since news gaps)))))

(defun wake-news (since)
  "(values NEWS GAPS): the lines *WAKE-SOURCES* report since SINCE, and one
line for each source that could not say. Only NEWS counts toward waking."
  (let ((news '())
        (gaps '()))
    (dolist (source *wake-sources*)
      (handler-case
          (let ((lines (funcall source since)))
            (if (eq lines :unavailable)
                (push (format nil "~a: unavailable" (source-label source)) gaps)
                (dolist (line lines)
                  (push (nlk:first-line (princ-to-string line) +line-limit+) news))))
        (error (condition)
          (push (format nil "~a: unavailable (~a)" (source-label source)
                        (nlk:first-line (princ-to-string condition) 160))
                gaps))))
    (values (nreverse news) (nreverse gaps))))

;;; --- spend --------------------------------------------------------------------------

(defun iso-utc (universal)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))

;; WAKE-SPENT-24H, the reader the allowance gates on, is observe.lisp's:
;; the wake's session and every session under it, off the event log.

;;; --- the due instant ----------------------------------------------------------------

(defun wake-skip (job now status &key detail backoff warn)
  "Pass the due instant without a fire: record why, and set the next one --
the doubled gap after a quiet check, the longest gap after a money gate."
  (let ((gap (if backoff
                 (min (wake-get job :max) (* 2 (wake-get job :gap)))
                 (wake-get job :gap))))
    (wake-put job :gap gap :cause :timer :asked nil)
    (setf (job-next-at job) (+ now (if backoff gap (wake-get job :max))))
    (set-last job now "skipped" (format nil "~a~@[: ~a~]" status detail))
    (when warn
      (post job :warning "cron wake skipped: ~a~@[: ~a~]" status detail))
    nil))

(defun wake-context (job now news gaps spent)
  "The run context a wake's prompt carries: the cause, the last wake, the
bounds, the spend, then the news."
  (let ((since (wake-get job :cursor))
        (allowance (wake-get job :allowance)))
    (with-output-to-string (out)
      (format out "wake ~d; cause: ~a~@[ (~a)~]~%"
              (1+ (or (wake-get job :wakes) 0))
              (case (wake-get job :cause)
                (:asked "the time you picked")
                (:now "the operator woke you")
                (t "your clock"))
              (and (eq (wake-get job :cause) :asked) (wake-get job :why)))
      (format out "last wake: ~:[none yet~;~:*~a~]~@[; last said (~a)~]~@[: ~a~]~%"
              (and since (left-text since now))
              (nlk:when-let (at (wake-get job :said-at)) (left-text at now))
              (wake-get job :said))
      (format out "bounds ~a to ~a, gap now ~a, ~d tool calls a wake~%"
              (duration-text (wake-get job :min)) (duration-text (wake-get job :max))
              (duration-text (wake-get job :gap)) (wake-get job :calls))
      (format out "spend over 24h: ~:[unknown~;~:*~a~]~@[ of an allowance of ~a~]~%"
              (and spent (usd spent)) (and allowance (usd allowance)))
      (if news
          (format out "news since ~:[the wakes began~;~:*~a~]:~%~{- ~a~%~}"
                  (and since (local-text since)) news)
          (format out "no news since ~:[the wakes began~;~:*~a~]~%" (and since (local-text since))))
      (when gaps
        (format out "could not read: ~{~a~^; ~}~%" gaps)))))

(defun wake-due (job now &key forced)
  "The wake job's due instant at NOW -- or the operator's wake, FORCED:
the money gates, the news gate unless the wake asked for this instant or
FORCED, then the fire. Returns the FIRE, or NIL when none was submitted.
Caller holds the lock."
  (let* ((session (job-session job))
         (allowance (wake-get job :allowance))
         (spent (wake-spent-24h session now)))
    (cond
      ((and allowance (null spent))
       (wake-skip job now "spend unknown"
                  :detail (format nil "a round of ~a in the last 24h has no price, and the allowance is ~a"
                                  session (usd allowance))
                  :warn t))
      ((and allowance (>= spent allowance))
       (wake-skip job now "allowance spent"
                  :detail (format nil "~a of ~a in 24h" (usd spent) (usd allowance))
                  :warn t))
      (t
       (let ((since (or (wake-get job :cursor) (job-created-at job))))
         (multiple-value-bind (news gaps)
             (if (eql (first *wake-read*) since)
                 (values (second *wake-read*) (third *wake-read*))
                 (wake-news since))
           ;; The first wake always fires; after it, the timer's own
           ;; instants wait for news or the longest gap.
           (if (and (null news)
                    (not forced)
                    (not (eq (wake-get job :cause) :asked))
                    (wake-get job :fired-at)
                    (< (- now (wake-get job :fired-at)) (wake-get job :max)))
               (wake-skip job now "no news" :backoff t)
               (let ((extra (wake-context job now news gaps spent)))
                 (wake-put job :asked nil)
                 (nlk:when-let (fire (fire-job job now :extra extra :manual forced))
                   (wake-put job :cursor now :fired-at now :cause :timer)
                   (setf (job-next-at job) (wake-safety-next job now))
                   fire)))))))))

(defun wake-settled (job status answer now)
  "A wake's fire ended with STATUS and ANSWER at NOW: count it, keep what it
said, and set its next instant -- the delay it picked, else the gap doubled
after a silent or failed wake, the shortest after one that spoke. A wake
turned off meanwhile stays off."
  (let* ((spoke (and (equal status "turn.completed") (not (silent-answer-p answer))))
         (asked (wake-get job :asked))
         (gap (cond (spoke (wake-get job :min))
                    (t (min (wake-get job :max) (* 2 (wake-get job :gap)))))))
    (wake-put job :wakes (1+ (or (wake-get job :wakes) 0)) :gap gap)
    (when spoke
      (wake-put job :said (nlk:first-line (nlk:trimmed answer) +line-limit+) :said-at now))
    (when (eq (job-state job) :active)
      ;; The gap counts from the wake's start, so the cadence holds; a picked
      ;; delay from now. Either way the next wake is at least the shortest gap
      ;; after this one ended: a long wake never runs into the next.
      (setf (job-next-at job) (cl:max (if asked
                                          (+ now asked)
                                          (+ (or (wake-get job :fired-at) now) gap))
                                      (+ now (wake-get job :min))))
      (wake-put job :cause (if asked :asked :timer) :asked nil))
    (unless asked
      (wake-put job :why nil))))

;;; --- the observe level ----------------------------------------------------------------

;; WAKE-CALL-REFUSAL, the observe level itself, is observe.lisp's: a
;; whitelist walk of the snippet's forms, never a match on its text.

(defun wake-gate (op next)
  "The :TOOL point: in the wake's session, a call WAKE-CALL-REFUSAL refuses
answers that refusal and does not run."
  (nlk:if-let (refusal (and (wake-session-p nlk:*scribe-session-id*) (wake-call-refusal op)))
    (nle:failure (format nil "ERROR: refused by the wake's observe level: ~a; the call did not run. A wake reads and thinks; say in your answer what should change." refusal))
    (funcall next op)))

;;; --- the verbs ------------------------------------------------------------------------

(defun cancel-wake-turn (session turn-id)
  "Ask the wake's running turn to stop; the seam a test stubs."
  (nlk:request-cancel-turn session turn-id "wakes turned off"))

(defun wake-status-text (now)
  (let ((job (wake-job)))
    (if (null job)
        "wakes are off and were never turned on; (cron:wake :on) turns them on"
        (let ((allowance (wake-get job :allowance))
              (spent (wake-spent-24h (job-session job) now)))
          (with-output-to-string (out)
            (if (eq (job-state job) :active)
                (format out "wakes on; next wake ~:[when the wake in flight settles~;~:*~a~]~@[ (picked: ~a)~]~%"
                        (and (job-next-at job) (not (wake-firing job)) (at-text (job-next-at job) now))
                        (and (eq (wake-get job :cause) :asked) (or (wake-get job :why) "no reason given")))
                (format out "wakes off~@[ since ~a~]~@[ (turned off from ~a)~]; (cron:wake :on) turns them on~%"
                        (nlk:when-let (at (wake-get job :off-at)) (local-text at)) (wake-get job :off-by)))
            (format out "bounds ~a to ~a, gap now ~a, ~d tool calls and ~a a wake~%"
                    (duration-text (wake-get job :min)) (duration-text (wake-get job :max))
                    (duration-text (wake-get job :gap)) (wake-get job :calls)
                    (duration-text (job-timeout-seconds job)))
            (format out "allowance ~:[none~;~:*~a a day~], spent ~:[unknown~;~:*~a~] in 24h~%"
                    (and allowance (usd allowance)) (and spent (usd spent)))
            (format out "~d wake~:p so far; last outcome: ~a~@[; last said: ~a~]~%"
                    (or (wake-get job :wakes) 0) (last-text job) (wake-get job :said))
            (format out "session ~a" (job-session job)))))))

(defun wake-on (now &key (min nil min-p) (max nil max-p) (allowance nil allowance-p) (calls nil calls-p)
                         timeout-minutes model provider prompt)
  (let* ((existing (find-job +wake-id+))
         (old (and existing (job-wake existing))))
    (when (and existing (null old))
      (fail "job ~s exists and is not the wake; (cron:remove ~s) frees the id" +wake-id+ +wake-id+))
    (let ((min (if min-p (seconds-from min ":min") (or (getf old :min) (getf +wake-defaults+ :min))))
          (max (if max-p (seconds-from max ":max") (or (getf old :max) (getf +wake-defaults+ :max))))
          (allowance (if allowance-p allowance (getf old :allowance)))
          (calls (if calls-p calls (or (getf old :calls) (getf +wake-defaults+ :calls)))))
      (when (< min 60)
        (fail ":min under a minute is refused: a wake is a model turn"))
      (when (< max min)
        (fail ":max ~a is shorter than :min ~a" (duration-text max) (duration-text min)))
      (unless (or (null allowance) (and (realp allowance) (plusp allowance)))
        (fail ":allowance is US dollars per 24 hours, a positive number, or NIL for none, not ~s" allowance))
      (unless (and (integerp calls) (plusp calls))
        (fail ":calls is a positive whole number of tool calls a wake may make, not ~s" calls))
      (when prompt (check-prompt prompt))
      (let ((job (or existing
                     (add-job (duration-text min) (or prompt *wake-prompt*) :name +wake-id+
                              :timeout-minutes timeout-minutes :model model :provider provider
                              :now now))))
        (unless (string= (job-id job) +wake-id+)
          (fail "the wake job could not take the id ~s" +wake-id+))
        (when existing
          (when prompt (setf (job-prompt job) prompt))
          (when timeout-minutes (setf (job-timeout job) (timeout-seconds-from-minutes timeout-minutes)))
          (when (and (or model provider) (nlk:store-open-p))
            (nlk:record-session-model-selection (job-session job) :provider provider :model model)))
        (setf (job-wake job) (append (list :min min :max max :calls calls) (job-wake job)))
        (wake-put job :min min :max max :calls calls :allowance allowance
                      :gap (clamp-gap (or (getf old :gap) min) min max)
                      :off-by nil :off-at nil :asked nil :cause :timer)
        (setf (schedule-display (job-schedule job)) (wake-display job))
        (unless (and (eq (job-state job) :active) (wake-firing job))
          (setf (job-next-at job)
                (+ now (wake-get job :gap)
                   (if old 0 (funcall *wake-jitter* (max 1 (floor min 2)))))))
        (setf (job-state job) :active)
        (persist-jobs)
        (poke)
        (nle:keep-running "the wake")
        (wake-status-text now)))))

(defun clamp-gap (gap min max)
  (cl:max min (cl:min max gap)))

(defun wake-off (now)
  (let ((job (wake-job)))
    (unless job
      (fail "wakes are not on; (cron:wake :on) turns them on"))
    (setf (job-state job) :paused)
    (wake-put job :off-by (or nlk:*scribe-session-id* "the operator") :off-at now :asked nil)
    (let* ((fire (wake-firing job))
           (turn (and fire (fire-turn-id fire))))
      (when turn
        (nlk:with-handlers ((error (condition)
                              (warn "cron: the wake's turn ~a was not cancelled: ~a" turn condition)))
          (cancel-wake-turn (job-session job) turn)))
      (persist-jobs)
      (poke)
      (format nil "wakes off~@[; the wake in flight (turn ~a) was asked to stop~]; only (cron:wake :on) turns them on again"
              turn))))

(defun wake-now (now)
  (let ((job (wake-job)))
    (unless (and job (eq (job-state job) :active))
      (fail "wakes are off; (cron:wake :on) first"))
    (wake-put job :cause :now)
    (let ((fire (wake-due job now :forced t)))
      (persist-jobs)
      (poke)
      (if fire
          (format nil "woke into session ~a~@[ (turn ~a)~]" (job-session job) (fire-turn-id fire))
          (format nil "not woken: ~a" (last-text job))))))

(define-verb wake-verb (action &rest keys)
  "The whole of (cron:wake) behind an explicit clock: KEYS are (cron:wake)'s
and :NOW."
  (let* ((now (or (getf keys :now) (get-universal-time)))
         (*wake-read* (and (eq action :now) (wake-read-news now :any t))))
   (with-cron-lock
    (unless (eq action :status)
      (refuse-from-job-session))
    (case action
      (:on (apply #'wake-on now (alexandria:remove-from-plist keys :now)))
      (:off (wake-off now))
      (:status (wake-status-text now))
      (:now (wake-now now))
      (t (fail "(cron:wake ACTION ...): ACTION is :on, :off, :status or :now, not ~s" action))))))

(defun wake (action &rest keys &key min max allowance calls timeout-minutes model provider prompt)
  "Wake-ups of its own: (cron:wake :on :min \"15m\" :max \"4h\" :allowance 2 :calls 40)
turns them on, :off stops them until the next :on, :status reads them, :now wakes once.
Answers a string."
  (declare (ignore min max allowance calls timeout-minutes model provider prompt))
  (apply #'wake-verb action keys))

(define-verb wake-next-verb (delay why &key (now (get-universal-time)))
  "The whole of (cron:wake-next) behind an explicit clock."
  (with-cron-lock
    (let ((job (wake-job))
          (session nlk:*scribe-session-id*))
      (unless (and job (stringp session) (string= session (job-session job)))
        (fail "only a wake turn picks its next wake, from the wake's own session; the operator sets the bounds with (cron:wake :on :min ... :max ...)"))
      (let* ((asked (seconds-from delay "cron:wake-next"))
             (held (wake-clamp job asked))
             (reason (and why (nlk:first-line (nlk:trimmed (princ-to-string why)) +line-limit+))))
        (wake-put job :asked held :why reason)
        ;; Outside a fire -- the operator working in the wake's session --
        ;; the clock moves now; inside one, WAKE-SETTLED moves it.
        (unless (wake-firing job)
          (setf (job-next-at job) (+ now held))
          (wake-put job :cause :asked :asked nil))
        (persist-jobs)
        (poke)
        (format nil "next wake in ~a~:[~*~*~*~; (asked ~a, held to the bounds ~a to ~a)~]~@[: ~a~]"
                (duration-text held) (/= held asked) (duration-text asked)
                (duration-text (wake-get job :min)) (duration-text (wake-get job :max)) reason)))))

(defun wake-next (delay &optional why)
  "From a wake turn: wake next in DELAY (\"2h\", \"45m\"), held to the operator's
bounds, WHY the reason the next wake reads. Answers a string."
  (wake-next-verb delay why))

;;; --- /wake ----------------------------------------------------------------------------

(defparameter +wake-usage+ "/wake [status | on | off | now]")

(defun wake-slash (args &aux (head (first (nlk:split-words args))))
  (cond ((or (null head) (string= head "status")) (wake :status))
        ((string= head "on") (wake :on))
        ((string= head "off") (wake :off))
        ((string= head "now") (wake :now))
        (t (format nil "wake: unknown subcommand ~a; usage ~a" head +wake-usage+))))
