;;;; cell.lisp --- one bounded second opinion when a tool call stops moving a task.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of rezaaibox-ui/nexus-recovery-peer's __init__.py. The Hermes
;;;; plugin watched pre_llm_call and transform_tool_result; a cell has one
;;;; point that sees both sides of a call, :TOOL, whose hook runs the call and
;;;; may rewrite the result. When the same call fails, or returns the same
;;;; thing, REPEAT_THRESHOLD times in a row, this cell asks a model on a
;;;; provider the user names in `peer' for at most two different next steps
;;;; and a way to check each, and appends them to the result the agent is
;;;; about to read. It never replays the call, never substitutes a model for
;;;; the session's own, and asks once per stall, so a loop costs one small
;;;; call rather than one per step.

(in-package #:nodecode-recovery-peer)

;;; --- the words ---------------------------------------------------------------

(defparameter +reviewer-instruction+
  (concatenate 'string
               "You are an independent recovery reviewer. Do not claim the task is solved. "
               "Give a DIFFERENT concrete next step (at most two) and how to verify it. "
               "Do not repeat the failed call unchanged. Never request or print credentials. "
               "Do not take actions or propose irreversible actions without human approval.")
  "The system message one review is asked under: the original plugin's own words.")

(defparameter +primer+
  "The recovery peer asks a second model -- on a provider of your own, named in
`peer' -- for one bounded way out of a tool loop. Nothing here needs calling:
when the same call fails, or returns the same thing, three times in a row (the
`repeat_threshold' setting), it asks once, for at most two different next steps
and how to check each, and appends them to the result the agent is about to
read. A stall costs one small call, not one per step; the agent still decides,
and nothing is treated as verified until it runs.

  unset peer              the cell refuses: name a peer in /settings
  /recovery-peer           on or off, the peer, this session's streak, the last word
  /recovery-peer on | off  watch tool results for a stall, or stop watching
  /recovery-peer reset     forget every streak and the last word
  /recovery-peer ask       ask the peer now, about the last stall this session had
  (help :recovery-peer)   the same, as prose

The peer sees the task, the call (eval(form), bounded) and how it ended -- the
failure text, or the fact that a read-only call returned the same output
again -- each clipped to `clip' characters and stripped of the obvious
credentials. Redaction is best-effort, not a security boundary: do not name a
peer whose provider should not see this conversation's error text. There is one
review_peer tool for asking by hand, and no background thread: the call happens
inside the tool dispatch that tripped it."
  "What (help :recovery-peer) answers while the cell runs.")

;;; --- the cell's own state ----------------------------------------------------

(defvar *lock* (sb-thread:make-mutex :name "recovery-peer")
  "One lock: two tool calls of one batch may land at once.")

(defvar *sessions* (make-hash-table :test #'equal)
  "Session id -> its record of repeated calls. Live-only: a restart forgets it.")

(defvar *peer-call* nil
  "A function of (SYSTEM USER) answering a review, which a test binds; NIL
asks the real peer through the organism's own side call.")

(defvar *last-review* ""
  "The last suggestion a peer gave, or \"\" while it has given none.")

(defvar *last-skip* ""
  "Why the last review was not asked, when it was not, in one line.")

(defvar *enabled* nil
  "Whether the :TOOL hook asks: the section's `enabled' to begin with, and
/recovery-peer on or off turns it either way without a restart.")

(defmacro with-state (&body body)
  "Run BODY holding the cell's lock."
  `(sb-thread:with-mutex (*lock*) ,@body))

;;; --- the peer ----------------------------------------------------------------

(defun split-peer (peer)
  "PEER as (values PROVIDER MODEL): `provider/model', cut at the first slash.
=> (values NIL NIL) when PEER is not a pair."
  (when (and (stringp peer) (plusp (length peer)))
    (let ((slash (position #\/ peer)))
      (when (and slash (plusp slash) (< (1+ slash) (length peer)))
        (values (subseq peer 0 slash) (subseq peer (1+ slash)))))))

(defun peer-target ()
  "The review's (values PROVIDER MODEL), from the section's `peer'."
  (split-peer (setting :peer)))

(defun ask-model (system user)
  "One side call to the peer, on the provider and model the section names.
A test binds *PEER-CALL*; the real path pins the organism's auxiliary target
for the length of the call, so no other session's target is touched."
  (if *peer-call*
      (funcall *peer-call* system user)
      (multiple-value-bind (provider model) (peer-target)
        (let ((nle::*auxiliary-model* (list :provider provider :model model)))
          (nle:complete system user :max-tokens (setting :max-tokens) :fallback-p nil)))))

(defun independent-p (provider)
  "Whether PROVIDER is a lane of its own: not the one this session runs on."
  (let ((own (nle:auxiliary-model)))
    (or (null own) (not (stringp own)) (not (string= own provider)))))

(defun why-not-review ()
  "Why no review was asked, in one line a model or an operator can act on."
  (multiple-value-bind (provider model) (peer-target)
    (cond ((or (null provider) (null model))
           "the recovery-peer section names no peer; set peer to provider/model")
          ((and (setting :other-provider-only) (not (independent-p provider)))
           (format nil "the peer ~a is the provider this session runs on" provider))
          (t (if (plusp (length *last-skip*)) *last-skip* "the peer gave no advice")))))

;;; --- what a peer is allowed to see --------------------------------------------

(defparameter +pem-begin+ "-----BEGIN " "Where a PEM block opens.")
(defparameter +pem-end+ "-----END " "Where its one-line body stops being secret.")

(defparameter +credential-prefixes+ '("sk-" "AIza" "ghp_" "gho_" "xoxb-" "AKIA")
  "Prefixes that make a word a token on sight, the original's own set.")

(defparameter +credential-labels+
  '("password" "passwd" "secret" "token" "api_key" "apikey" "api-key"
    "authorization" "bearer" "access_token" "client_secret")
  "A word naming a credential: the word after it is replaced.")

(defun clip (text limit)
  "TEXT on one line, at most LIMIT characters, an ellipsis where it was cut."
  (let ((one (or (ignore-errors (nlk:one-line text)) "")))
    (if (> (length one) limit)
        (concatenate 'string (subseq one 0 (max 0 (1- limit))) "...")
        one)))

(defun pem-scrub (text)
  "TEXT with every PEM block replaced by [redacted], up to its END line."
  (let ((start (search +pem-begin+ text)))
    (if (null start)
        text
        (let* ((end (search +pem-end+ text :start2 start))
               ;; The block ends at the END line's own closing dashes; an
               ;; unterminated one runs to the end of the text.
               (marker (and end (search "-----" text :start2 (+ end (length +pem-end+)))))
               (stop (if (and marker (plusp marker))
                         (+ marker 5)
                         (length text))))
          (concatenate 'string (subseq text 0 start) "[redacted]"
                       (pem-scrub (subseq text stop)))))))

(defun label-word-p (word)
  "Whether WORD names a credential, case folded and stripped of punctuation."
  (let ((clean (string-downcase (string-right-trim ":=,;'\"" word))))
    (member clean +credential-labels+ :test #'string=)))

(defun token-word-p (word)
  "Whether WORD carries a token prefix."
  (some (lambda (prefix) (uiop:string-prefix-p prefix word)) +credential-prefixes+))

(defun redact (text &key limit)
  "TEXT as the peer may see it: a PEM block, a word carrying a token prefix,
and the word after a credential label each become [redacted]. Best-effort, not
a boundary. LIMIT cuts the answer."
  (let* ((text (pem-scrub (or (and (stringp text) text) "")))
         (redacted
           (with-output-to-string (out)
             (let ((word '()) (previous ""))
               (labels ((flush ()
                          (when word
                            (let ((one (coerce (nreverse word) 'string)))
                              (write-string (if (or (token-word-p one)
                                                    (and (plusp (length previous))
                                                         (label-word-p previous)))
                                                "[redacted]" one)
                                            out)
                              (setf previous one word '())))))
                 (loop for char across text
                       do (if (member char '(#\Space #\Tab #\Newline #\Return))
                              (progn (flush) (write-char char out))
                              (push char word)))
                 (flush))))))
    (if limit (subseq redacted 0 (min limit (length redacted))) redacted)))

;;; --- one finished call --------------------------------------------------------

(defun current-session ()
  "The session the running turn belongs to, or \"local\" outside a turn."
  (or (ignore-errors (and (boundp 'nlk:*scribe-session-id*)
                          (symbol-value 'nlk:*scribe-session-id*)))
      (ignore-errors (getf (nle:turn) :session-id))
      "local"))

(defun failed-result-p (result)
  "Whether RESULT is a tool result that says it failed."
  (and (stringp result)
       (or (and (boundp 'nodecode.evolved::*tool-call-failed*)
                (symbol-value 'nodecode.evolved::*tool-call-failed*))
           (uiop:string-prefix-p "ERROR" result))))

(defun call-label (name arguments)
  "How one call is named to the peer: eval(form), bounded."
  (let ((form (and (hash-table-p arguments) (gethash "form" arguments))))
    (if (and (stringp form) (plusp (length form)))
        (format nil "~a(~a)" name (clip form 120))
        name)))

(defun call-category (name result)
  "What a finished call is: :FAILED, :QUIET for a tool whose identical success
is no progress, or NIL for a call that moved the task."
  (cond ((failed-result-p result) :failed)
        ((member name (setting :no-progress-tools) :test #'string=) :quiet)
        (t nil)))

(defun argument-blob (arguments)
  "ARGUMENTS as one comparable string: the form it runs and its strings.
Read by content, not by a hash table's printed address, so two calls that
say the same thing are the same call."
  (if (hash-table-p arguments)
      (format nil "~a|~a"
              (let ((form (gethash "form" arguments)))
                (if (stringp form) form (format nil "~s" form)))
              (let ((strings (gethash "strings" arguments)))
                (or (and strings
                         (handler-case (nlk:encode-json-object strings)
                           (error () (format nil "~s" strings))))
                    "")))
      (format nil "~s" arguments)))

(defun signature (name arguments result category)
  "Two results are the same stall when they share all of this."
  (list category name (sxhash (format nil "~a|~a" (argument-blob arguments) result))))

(defun record-for (session)
  "SESSION's record, made on first sight."
  (or (gethash session *sessions*)
      (setf (gethash session *sessions*)
            (list :signature nil :streak 0 :asked nil :asked-at 0 :reviews 0
                  :last ""))))

(defun reset-state ()
  "Forget every streak, the last review and its refusal: a start, or a reset."
  (with-state
    (clrhash *sessions*)
    (setf *last-review* "" *last-skip* "")))

(defun decide-review (session name arguments result category)
  "Whether this result is the one that earns a review.

Counts identical results in a row; answers T on the REPEAT_THRESHOLD one,
once per stall and once per COOLDOWN_SECONDS. The flag is raised before the
call, the way the original reserved its slot, so a batch of identical calls
landing together asks once."
  (with-state
    (let* ((record (record-for session))
           (signature (signature name arguments result category))
           (same (equal signature (getf record :signature)))
           (streak (if same (1+ (getf record :streak)) 1))
           (now (get-universal-time)))
      (setf (getf record :signature) signature
            (getf record :streak) streak)
      (unless same (setf (getf record :asked) nil))
      (let* ((threshold (setting :repeat-threshold))
             (cooldown (setting :cooldown-seconds))
             (ask (and (>= streak threshold)
                       (not (getf record :asked))
                       (or (zerop cooldown)
                           (>= now (+ (getf record :asked-at) cooldown))))))
        (when ask
          (setf (getf record :asked) t
                (getf record :asked-at) now))
        (setf (gethash session *sessions*) record)
        ask))))

(defun review-request (task failure tool)
  "What the peer is shown: the task, the call, and how it ended, each bounded."
  (format nil "Task: ~a~%Tool: ~a~%Observed failure/no-progress: ~a"
          (redact (or task "(task not available)") :limit 1000)
          (or (and (stringp tool) (plusp (length tool)) tool) "(tool not named)")
          (redact (or failure "(no failure text)") :limit (setting :clip))))

(defun review (task failure tool)
  "Ask the peer for one bounded next step.
=> (values TEXT PROVIDER MODEL), or NIL and a reason in WHY-NOT-REVIEW."
  (setf *last-skip* "")
  (multiple-value-bind (provider model) (peer-target)
    (when (and provider model
               (or (not (setting :other-provider-only)) (independent-p provider)))
      (let ((answer (handler-case (ask-model +reviewer-instruction+
                                            (review-request task failure tool))
                      (error (condition)
                        (setf *last-skip*
                              (format nil "the peer call failed (~a)" (type-of condition)))
                        nil))))
        (if (and (stringp answer)
                 (plusp (length (string-trim '(#\Space #\Tab #\Newline) answer))))
            (let ((text (redact answer :limit 1200)))
              (setf *last-review* text)
              (values text provider model))
            (progn (unless (plusp (length *last-skip*))
                     (setf *last-skip* "the peer answered nothing"))
                   nil))))))

(defun advice-line (text provider model)
  "How a suggestion reads in the result the agent is about to see."
  (format nil "[Recovery peer suggestion (~a/~a, not verified): ~a Verify it before claiming success.]"
          provider model text))

(defun session-task (session)
  "The operator's newest ask in SESSION, bounded, or NIL when the log cannot say."
  (ignore-errors
   (loop for row in (coerce (nlk:recall-log :session-id session :limit 40 :clip 1000) 'list)
         when (equal "user" (getf row :role))
           do (return (clip (getf row :text) 1000)))))

(defun note-result (session name arguments result)
  "Record one finished call; answer the text to append to RESULT, or NIL."
  (when (and *enabled* (stringp result))
    (let ((category (call-category name result)))
      (when (and category
                 (decide-review session name arguments result category))
        (multiple-value-bind (text provider model)
            (review (or (getf (record-for session) :task) (session-task session))
                    (format nil "~(~a~): ~a" category (clip result (setting :clip)))
                    (call-label name arguments))
          (when text
            (let ((record (record-for session)))
              (setf (getf record :reviews) (1+ (getf record :reviews)))
              (advice-line text provider model))))))))

(defun watch-call (op next)
  "The :TOOL point: run the call, then append one review when it repeated.
Fail-open in both directions: an error here never fails the call, and a call
that never repeats is handed back exactly as it came."
  (let* ((name (or (getf op :name) "tool"))
         (arguments (getf op :arguments))
         (session (current-session))
         (values (multiple-value-list (funcall next op))))
    (handler-case
        (let* ((result (first values))
               (advice (note-result session name arguments result)))
          (when (and advice (stringp result))
            (setf (first values) (format nil "~a~%~%~a" result advice))))
      (error (condition)
        ;; The call has already happened: its result stands, and the reason no
        ;; review followed is what /recovery-peer reports.
        (setf *last-skip* (format nil "the review could not be asked (~a)" condition))))
    (values-list values)))

;;; --- the tool, by hand --------------------------------------------------------

(nlk:define-contract recovery-peer-ask
  (:doc "Ask a model on another provider for one bounded way out of a failure or a stall. It sees the task and the failure you describe, not the transcript; it answers at most two different next steps and how to check each, and nothing it says is verified until you run it. Use it when a tool keeps failing the same way, a read-only call returns the same thing again, or you are out of ideas.")
  ("task" :text :required t :doc "the task, in a sentence or two")
  ("failure" :text :required t :doc "what went wrong or stalled, and the call that produced it")
  ("tool" :string :default "" :doc "the tool that failed, as you called it"))

(defun ask-tool (ask)
  "The recovery_peer tool: one review of the failure the model names."
  (multiple-value-bind (text provider model)
      (review (recovery-peer-ask-task ask)
              (recovery-peer-ask-failure ask)
              (recovery-peer-ask-tool ask))
    (if text
        (advice-line text provider model)
        (format nil "no second opinion: ~a" (why-not-review)))))

;;; --- /recovery-peer -----------------------------------------------------------

(defun words-of (text)
  "TEXT split into words, the empty ones dropped."
  (remove "" (uiop:split-string (or text "") :separator '(#\Space #\Tab))
          :test #'string=))

(defun session-line (session)
  "What this session has been doing, as one line."
  (let* ((record (gethash session *sessions*))
         (streak (or (and record (getf record :streak)) 0))
         (asked (and record (getf record :asked))))
    (format nil "this session: ~d identical result~:p in a row~@[, reviewed~]"
            streak (and asked t))))

(defun status-text (&optional (session (current-session)))
  "The cell as it stands: the switch, the peer, the thresholds, the last word."
  (multiple-value-bind (provider model) (peer-target)
    (format nil "recovery peer ~a - peer ~a, model ~a, ~@[a provider of its own only - ~]~d identical results in a row ask once, ~d s between asks;~%~a~%~@[last suggestion: ~a~%~]~@[not asked: ~a~]"
            (if *enabled* "on" "off")
            (or provider "(none)")
            (or model "(none)")
            (setting :other-provider-only)
            (setting :repeat-threshold)
            (setting :cooldown-seconds)
            (session-line session)
            (and (plusp (length *last-review*)) (clip *last-review* 300))
            (and (plusp (length *last-skip*)) *last-skip*))))

(defun run-slash (args session-id)
  "The /recovery-peer command: status, on, off, reset, ask."
  (let* ((words (words-of args))
         (word (or (first words) "status")))
    (cond ((string= word "status") (status-text (or session-id (current-session))))
          ((string= word "on")
           (setf *enabled* t)
           (format nil "recovery peer on: ~d identical results in a row and it asks ~a~@[/~a~]."
                   (setting :repeat-threshold)
                   (multiple-value-bind (provider model) (peer-target)
                     (if provider (format nil "~a" provider) "(no peer named)"))
                   (multiple-value-bind (provider model) (peer-target)
                     (declare (ignore provider))
                     model)))
          ((string= word "off")
           (setf *enabled* nil)
           "recovery peer off: results pass through untouched; the tool and /recovery-peer ask still work.")
          ((string= word "reset")
           (reset-state)
           "recovery peer reset: every streak and the last word are forgotten.")
          ((string= word "ask")
           (multiple-value-bind (text provider model)
                 (review (or (session-task (or session-id (current-session))) "(task not available)")
                         "asked by hand, at a stall"
                         "(by hand)")
               (if text
                   (format nil "~a" (advice-line text provider model))
                   (format nil "no second opinion: ~a" (why-not-review)))))
          (t (format nil "~a is not one of its words: status, on, off, reset, ask" word)))))

;;; --- the cell -----------------------------------------------------------------

(defun read-settings (values table)
  "The section's values, refused when `peer' is not a provider/model pair.

The peer is the whole point of the cell: an unset or malformed one refuses
in the section's own words, so /cells shows REFUSED with the fix in the
notice instead of a cell that quietly watches and never asks."
  (declare (ignore table))
  (let ((peer (getf values :peer)))
    (multiple-value-bind (provider model) (split-peer peer)
      (unless (and provider model)
        (nlk:config-error "peer must be provider/model, e.g. some-lane/some-model; it is ~a"
                          (if (and (stringp peer) (plusp (length peer)))
                              (format nil "~s" peer)
                              "unset")))
      (list* :peer peer :provider provider :model model values))))

(nle:define-cell recovery-peer
  (:section ("recovery-peer")
    (:guide "peer is the identity of the second opinion, written provider/model of a model you have; repeat_threshold is how many identical results in a row, a failure or a read-only repeat, earn one review; nothing else is required")
    ("peer" :string :required t :doc "the second opinion, as provider/model")
    ("enabled" :boolean :default t :doc "watch tool results and ask on a stall")
    ("repeat_threshold" :integer :default 3 :min 2 :doc "identical results in a row before one review")
    ("no_progress_tools" :list :default '("eval") :doc "tools whose identical success is no progress")
    ("max_tokens" :integer :default 220 :min 32 :doc "the review's own ceiling")
    ("clip" :integer :default 350 :min 40 :doc "how much of a failure the peer reads")
    ("cooldown_seconds" :integer :default 120 :min 0 :doc "how long a session waits between reviews")
    ("other_provider_only" :boolean :default t :doc "ask only when the peer is on a provider this session does not run on"))
  (:settings #'read-settings)
  (:tool "recovery_peer" (nlk:contract 'recovery-peer-ask) 'ask-tool)
  (:help :recovery-peer "recovery-peer: one bounded second opinion from another model when a tool call stalls - /recovery-peer" +primer+)
  (:command "recovery-peer" 'run-slash
            :description "The peer on or off, its identity, and the last word it gave"
            :argument-hint "[status|on|off|reset|ask]")
  (:hook :tool #'watch-call)
  (:start (lambda ()
            (reset-state)
            (setf *enabled* (setting :enabled)))))
