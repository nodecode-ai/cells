;;;; cell.lisp --- one batch comparison, three bands, and a receipt of hashes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ordaprompt_router/router.py, adapter.py, receipts.py and the
;;;; CLI in cli.py. The original's whole agent-facing surface was one command:
;;;; a request, a candidate set, one batch comparison per surface, a decision
;;;; in one of three bands, and a receipt that carried hashes and scores and
;;;; never the prompt. A Nodecode cell says the same thing as a tool the model
;;;; calls and a /ordaprompt command the operator types.
;;;;
;;;; One thing changes hands and worth naming: the original's default backend
;;;; scored a prompt by hashing it, so its numbers had no relation to what the
;;;; prompt said. Here the backend is lexical -- a candidate scores by how
;;;; much of itself the prompt carries -- and the policy above it is the
;;;; original's, band for band. The adapter seam survives as the input: a
;;;; caller that has its own classifier still sends candidates, and the bands,
;;;; margins and receipts are the ones the original defines.

(in-package #:nodecode-ordaprompt)

(nlk:define-peripheral ordaprompt :not-running t)

;;; --- the closed domains, as data ---------------------------------------------

(defparameter *contamination-weights*
  '(("project_mismatch" . 0.7) ("secret_presence" . 0.5)
    ("topic_drift" . 0.4) ("stale_context" . 0.2))
  "What one contamination flag costs a session's utility (K_j in the original).
A flag outside this list is worth nothing, because the domain is closed.")

(defparameter *novel* "novel"
  "The sentinel candidate: no kept topic fits, so propose one.")

(defparameter *ambiguous* "ambiguous"
  "The sentinel candidate: the top two real candidates are tied.")

(defparameter *new-session* "new_session"
  "The sentinel candidate: start a session of its own.")

(defparameter *router-version* "1.0.0-port"
  "What a receipt says wrote it: the original's router version, ported.")

(defparameter *stop-words*
  '("the" "and" "for" "with" "that" "this" "from" "into" "about" "have" "has"
    "was" "were" "are" "not" "but" "you" "your" "its" "can" "all" "any" "our"
    "out" "one" "how" "why" "when" "what" "which" "who" "then" "than" "them"
    "they" "there" "here" "also" "just" "please" "would" "could" "should"
    "does" "did" "use" "using" "get" "got" "make" "needs" "need" "again")
  "Words a prompt and a label share without meaning anything by it.")

;;; --- words --------------------------------------------------------------------

(defun threshold (name)
  "A threshold as the section holds it -- an integer in hundredths -- as the
fraction the policy compares against. The section has no float type, so every
fraction the original's Thresholds dataclass carried is declared here as a
percentage, which is also what an operator can read at a glance."
  (/ (setting name) 100.0))

(defun tokenize (text)
  "TEXT's words that carry meaning: split on anything that is not a letter or a
digit, downcased, stop words and words under three letters dropped, in order."
  (let ((words '())
        (start nil))
    (labels ((cut (end)
               (when start
                 (let ((word (string-downcase (subseq text start end))))
                   (unless (or (< (length word) 3) (member word *stop-words* :test #'string=))
                     (pushnew word words :test #'string=)))
                 (setf start nil))))
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (if (alphanumericp character)
                   (unless start (setf start index))
                   (cut index)))
      (cut (length text)))
    (nreverse words)))

(defun prefix-p (short long)
  "Whether SHORT is a prefix of LONG."
  (and (<= (length short) (length long))
       (string= short long :end2 (length short))))

(defun same-term-p (a b)
  "Whether two words are one term: equal, or one a prefix of the other and both
at least four letters, so `deploy' meets `deployment' and `bill' does not meet
`billing'."
  (or (string= a b)
      (and (>= (length a) 4)
           (>= (length b) 4)
           (or (prefix-p a b) (prefix-p b a)))))

(defun coverage (terms against)
  "How much of TERMS AGAINST carries, in [0,1]: the fraction of a candidate's
own words that appear in the text. A candidate the text names whole scores 1.0
however long the text is, and a candidate it does not name scores 0.0."
  (if (null terms)
      0.0
      (/ (count-if (lambda (term) (some (lambda (other) (same-term-p term other)) against))
                   terms)
         (float (length terms)))))

(defun rank-scores (scores)
  "SCORES, a list of (NAME . VALUE), best first, ties broken by name so the
ranking is the same on every run."
  (sort (copy-list scores)
        (lambda (a b)
          (or (> (cdr a) (cdr b))
              (and (= (cdr a) (cdr b)) (string< (car a) (car b)))))))

;;; --- the two surfaces ---------------------------------------------------------

(defun score-topics (prompt-terms topics)
  "Every topic candidate and both topic sentinels, in one list of (NAME . SCORE).
A candidate's score is how much of its own label the prompt carries. `novel' is
the residual -- 1.0 less the best real candidate -- and `ambiguous' is how tied
the top two are once a real candidate actually scores: 1.0 at a dead heat, 0.0
once they are a full point apart, and 0.0 when nothing scores at all, because
a tie at zero is novelty and not a choice between two labels."
  (let* ((scores (loop for label in topics collect (cons label (coverage (tokenize label) prompt-terms))))
         (ranked (rank-scores scores))
         (best (if ranked (cdr (first ranked)) 0.0))
         (second (if (cdr ranked) (cdr (second ranked)) 0.0))
         (margin (if (cdr ranked) (- best second) 1.0)))
    (append scores
            (list (cons *novel* (- 1.0 best))
                  (cons *ambiguous* (if (and (cdr ranked) (plusp best))
                                        (max 0.0 (min 1.0 (- 1.0 margin)))
                                        0.0))))))

(defun contamination-risk (flags)
  "K_j = 1 - prod(1 - k_j) over the flags a session carries."
  (- 1.0 (reduce (lambda (survival flag)
                   (* survival (- 1.0 (or (cdr (assoc flag *contamination-weights* :test #'string=))
                                          0.0))))
                 flags :initial-value 1.0)))

(defun score-sessions (prompt-terms sessions)
  "One row per session candidate plus the new-session sentinel, and the margin
between the top utility and its nearest rival. A session's similarity is
coverage of its summary (its id when it has none); its utility is that
similarity less what reusing it costs in context and risks in contamination.
The sentinel's utility is the section's fixed price for a fresh session."
  (let* ((rows (loop for session in sessions
                      for name = (getf session :id)
                      for terms = (tokenize (let ((summary (getf session :summary)))
                                              (if (and (stringp summary) (plusp (length summary)))
                                                  summary
                                                  name)))
                      for raw = (coverage terms prompt-terms)
                      for cost = (min 1.0 (/ (getf session :tokens 0)
                                             (float (setting :context-cost-norm))))
                      for risk = (contamination-risk (getf session :flags))
                      collect (list :name name :raw raw :risk risk
                                    :cost cost
                                    :utility (- raw
                                                (* (threshold :w-cost) cost)
                                                (* (threshold :w-contam) risk)))))
         (new (list :name *new-session*
                    :raw (- 1.0 (reduce #'max rows :key (lambda (row) (getf row :raw))
                                        :initial-value 0.0))
                    :risk 0.0 :cost 0.0
                    :utility (threshold :new-session-utility)))
         (ranked (sort (cons new rows)
                       (lambda (a b)
                         (or (> (getf a :utility) (getf b :utility))
                             (and (= (getf a :utility) (getf b :utility))
                                  (string< (getf a :name) (getf b :name)))))))
         (rest-utility (reduce #'max (rest ranked) :key (lambda (row) (getf row :utility))
                               :initial-value 0.0)))
    (values ranked (- (getf (first ranked) :utility) rest-utility))))

;;; --- the bands ------------------------------------------------------------------

(defun band-order (band)
  "How conservative BAND is: the higher the number, the less it claims."
  (cond ((string= band "automatic") 0)
        ((string= band "fallback_escalate") 1)
        (t 2)))

(defun topic-band (ranked)
  "The topic side's band and escalation code, as the original's table has them:
with no validated calibration nothing is automatic; an ambiguity that wins
escalates; a novel winner needs its own threshold and margin; anything under
tau_low abstains instead of guessing."
  (let* ((top (first ranked))
         (second-entry (second ranked))
         (top-score (cdr top))
         (margin (if second-entry (- top-score (cdr second-entry)) 1.0))
         (ambiguous (or (cdr (assoc *ambiguous* ranked :test #'string=)) 0.0)))
    (cond
      ((not (setting :calibration-active))
       (values (if (< top-score (threshold :tau-low)) "abstain_or_new_session" "fallback_escalate")
               "calibration_missing"))
      ((and (member *ambiguous* (list (car top) (and second-entry (car second-entry))) :test #'string=)
            (>= ambiguous 0.40))
       (values "fallback_escalate" "ambiguous_won"))
      ((string= (car top) *novel*)
       (cond ((and (>= top-score (threshold :tau-novel)) (>= margin (threshold :mu-novel)))
              (values "automatic" nil))
             ((< top-score (threshold :tau-novel)) (values "fallback_escalate" "novel_below_threshold"))
             (t (values "fallback_escalate" "below_margin"))))
      ((< top-score (threshold :tau-low)) (values "abstain_or_new_session" "below_min_score"))
      ((and (>= top-score (threshold :tau-topic)) (>= margin (threshold :mu-topic)))
       (values "automatic" nil))
      ((>= top-score (threshold :tau-topic)) (values "fallback_escalate" "below_margin"))
      (t (values "fallback_escalate" "below_min_score")))))

(defun session-band (ranked margin)
  "The session side's band and code. Reuse is stricter than a topic: a
contaminated session never wins, and a near tie abstains rather than reusing."
  (let ((top (first ranked))
        (real (remove *new-session* ranked :key (lambda (row) (getf row :name)) :test #'string=)))
    (cond
      ((null ranked) (values "abstain_or_new_session" "new_session_argmax"))
      ((not (setting :calibration-active)) (values "abstain_or_new_session" "calibration_missing"))
      ((string= (getf top :name) *new-session*)
       (values "abstain_or_new_session" "new_session_argmax"))
      ((and real (>= (getf (first (sort (copy-list real) #'> :key (lambda (row) (getf row :raw))))
                           :risk)
                     (threshold :k-block)))
       (values "abstain_or_new_session" "contamination_high"))
      ((< (getf top :raw) (threshold :tau-sess-sim))
       (values "abstain_or_new_session" "below_min_score"))
      ((< (getf top :utility) (threshold :tau-sess-util))
       (values "abstain_or_new_session" "below_min_utility"))
      ((< margin (threshold :mu-sess)) (values "abstain_or_new_session" "below_margin"))
      (t (values "automatic" nil)))))

(defun effective-band (topic topic-code session session-code)
  "The more conservative of the two bands, with the code that explains it."
  (cond ((null session) (values topic topic-code))
        ((> (band-order session) (band-order topic)) (values session session-code))
        ((< (band-order session) (band-order topic)) (values topic topic-code))
        (t (values topic (or topic-code session-code)))))

(defun decision-for (band topic-top session-ranked session-band)
  "What the band means to a caller: a topic, a reused session, a proposed new
topic, an escalation, or a session of its own."
  (cond
    ((string= band "automatic")
     (cond ((and topic-top (string= (car topic-top) *novel*)) "novel_propose")
           ((and (string= session-band "automatic")
                 (getf (first session-ranked) :name)
                 (not (string= (getf (first session-ranked) :name) *new-session*)))
            "session_reuse")
           (t "topic_assign")))
    ((string= band "fallback_escalate") "escalate")
    (t "new_session")))

;;; --- the receipt ------------------------------------------------------------------

(defun receipt-object (result)
  "RESULT as a receipt: closed fields, hashes, labels and scores, and never the
prompt, a file or a path. The original's ReceiptStore refuses free text; this
one cannot carry any, because there is nowhere in it to put a word from the ask."
  (nlk:plist-json
        (list :receipt_id (getf result :receipt-id)
              :at (nlk:iso-now)
              :router_version *router-version*
              :prompt_hash (getf result :prompt-hash)
              :band (getf result :band)
              :decision (getf result :decision)
              :escalation_code (or (getf result :code) "none")
              :topic_band (getf result :topic-band)
              :session_band (or (getf result :session-band) "none")
              :topic_margin (float (getf result :topic-margin))
              :session_margin (float (or (getf result :session-margin) 0.0))
              :topic_scores (map 'vector (lambda (row) (list :candidate (car row) :raw (float (cdr row))))
                                 (getf result :topic-ranked))
              :session_scores (map 'vector (lambda (row) (list :candidate (getf row :name)
                                                               :raw (float (getf row :raw))
                                                               :utility (float (getf row :utility))
                                                               :risk (float (getf row :risk))))
                                   (getf result :session-ranked))
              :contamination_score (float (getf result :contamination))
              :token_cost_estimate (getf result :tokens))))

(defun receipt-line (path result)
  "Append RESULT's receipt to PATH and answer PATH when it was written, NIL when
no path is set, and a line saying why when the write failed: a receipt the
caller believes in and the file does not hold is worse than none."
  (cond
    ((not (and (stringp path) (plusp (length path)))) nil)
    (t (handler-case (progn (nlk:append-json-line path (receipt-object result))
                            path)
         (error (condition)
           (format nil "not written (~a)" (type-of condition)))))))

(defun receipt-id (prompt)
  "A receipt's own id: a digest, so nothing of the prompt is legible in it."
  (let ((hash (nlk:sha256-text (format nil "~a|~a|~a" prompt (nlk:iso-now) (random 1000000000)))))
    (if (and (stringp hash) (> (length hash) 20)) (subseq hash 7 23) hash)))

;;; --- the decision -------------------------------------------------------------------

(defun route (prompt topics sessions &key (receipts-file (setting :receipts-file)))
  "One routing decision. PROMPT is the ask; TOPICS are the kept labels it could
belong to; SESSIONS are the live ones it could join, each a plist of :ID,
:SUMMARY, :TOKENS and :FLAGS. Answers the result plist, and appends a
hashes-only receipt to RECEIPTS-FILE when one is set."
  (let* ((prompt (or prompt ""))
         (topics (remove-if-not #'stringp topics))
         (topic-scored (score-topics (tokenize prompt) topics))
         (topic-ranked (rank-scores topic-scored))
         (topic-margin (if (second topic-ranked) (- (cdr (first topic-ranked)) (cdr (second topic-ranked))) 1.0)))
    (multiple-value-bind (topic-band-value topic-code) (topic-band topic-ranked)
      (multiple-value-bind (session-ranked session-margin) (score-sessions (tokenize prompt) sessions)
        (multiple-value-bind (session-band-value session-code)
            (if sessions (session-band session-ranked session-margin) (values nil nil))
          (multiple-value-bind (band code)
              (effective-band topic-band-value topic-code session-band-value session-code)
            (let* ((decision (decision-for band (first topic-ranked) session-ranked session-band-value))
                   (real-sessions (remove *new-session* session-ranked
                                          :key (lambda (row) (getf row :name)) :test #'string=))
                   (reused (and (string= decision "session_reuse") (first session-ranked)))
                   (result (list :prompt-hash (nlk:sha256-text prompt)
                                 :receipt-id (receipt-id prompt)
                                 :band band :decision decision :code code
                                 :topic-band topic-band-value :topic-code topic-code
                                 :session-band (and sessions session-band-value)
                                 :session-code (and sessions session-code)
                                 :topic-ranked topic-ranked :topic-margin topic-margin
                                 :session-ranked (and sessions session-ranked)
                                 :session-margin (and sessions (float session-margin))
                                 :contamination (if real-sessions
                                                    (getf (first (sort (copy-list real-sessions)
                                                                       #'> :key (lambda (row) (getf row :raw))))
                                                          :risk)
                                                    0.0)
                                 :tokens (if reused (getf reused :tokens) 0))))
              (setf (getf result :receipt) (receipt-line receipts-file result))
              result)))))))

;;; --- what a caller reads --------------------------------------------------------------

(defun percent (value) (format nil "~,2f" (float value)))

(defun topics-line (ranked)
  "The ranking, best first, with the margin that made the band."
  (format nil "~{~a~^, ~}"
          (loop for (name . score) in ranked
                collect (format nil "~a ~a" name (percent score)))))

(defun sessions-line (ranked)
  "Each session's utility, and what it costs and risks."
  (format nil "~{~a~^, ~}"
          (loop for row in ranked
                collect (format nil "~a ~a~:[~; , risk ~a~]"
                                (getf row :name) (percent (getf row :utility))
                                (plusp (getf row :risk)) (percent (getf row :risk))))))

(defun describe-result (result)
  "RESULT as the text a caller reads: the decision, the band and why, both
rankings, and where the receipt went."
  (format nil "~{~a~%~}"
          (remove nil
                  (list (format nil "decision ~a, band ~a~@[ (~a)~]"
                                (getf result :decision) (getf result :band) (getf result :code))
                        (format nil "topics: ~a" (topics-line (getf result :topic-ranked)))
                        (let ((ranked (getf result :session-ranked)))
                          (and ranked (format nil "sessions: ~a" (sessions-line ranked))))
                        (let ((path (getf result :receipt)))
                          (format nil "receipt: ~a" (or path "none (set receipts_file to keep one)")))
                        (format nil "bands: topic ~a~@[ (~a)~]~@[; session ~a~]~@[ (~a)~]"
                                (getf result :topic-band) (getf result :topic-code)
                                (getf result :session-band) (getf result :session-code))))))

;;; --- the tool --------------------------------------------------------------------------

(nlk:define-contract session-candidate
  (:doc "One live session this ask could join, with what joining it would cost and risk.")
  ("session_id" :text :required t :doc "the session's own id")
  ("summary" :string :default "" :doc "what that session is about, in a few words; its own id is read when empty")
  ("tokens" :integer :default 0 :doc "the context it already holds, in tokens")
  ("flags" :array :of :string :doc "contamination it carries: project_mismatch, secret_presence, topic_drift, stale_context"))

(nlk:define-contract route-arguments
  (:doc "Decide where an ask belongs before you route it. Pass the ask as prompt and the topic labels it could belong to; pass sessions when the ask might continue one instead of opening a new one. One batch comparison scores every candidate together, sentinels included, and the answer is fail-closed: automatic only for a calibrated, well-margined match; confirm/escalate in between; new_session or a novel proposal when nothing fits. It answers a decision, both ranked lists and the reason codes, never a guess.")
  ("prompt" :text :required t :doc "the ask, as the user made it, in a few sentences")
  ("topics" :array :of :string :doc "the kept topic labels it could belong to, e.g. (\"billing\" \"deploy\" \"billing invoices\")")
  ("sessions" :array :of session-candidate :doc "the live sessions it could continue; empty compares against a new session alone"))

(defun route-tool (args)
  "The ordaprompt_route tool: the decision, in words."
  (let ((prompt (route-arguments-prompt args))
        (topics (coerce (or (route-arguments-topics args) #()) 'list))
        (sessions (loop for session across (or (route-arguments-sessions args) #())
                         collect (list :id (session-candidate-session-id session)
                                       :summary (session-candidate-summary session)
                                       :tokens (session-candidate-tokens session)
                                       :flags (coerce (session-candidate-flags session) 'list)))))
    (if (and (null topics) (null sessions))
        "nothing to route: pass at least one topic label or session candidate"
        (describe-result (route prompt topics sessions)))))

;;; --- the command -------------------------------------------------------------------------

(defun split-on (text separator)
  "TEXT's pieces between SEPARATOR, trimmed, the empty ones dropped."
  (let ((pieces '())
        (start 0))
    (loop for index = (position separator text :start start)
          do (push (nlk:trimmed (subseq text start index)) pieces)
          while index
          do (setf start (1+ index)))
    (remove "" (nreverse pieces) :test #'string=)))

(defun status-text ()
  "What /ordaprompt says alone: how it is configured, and how to use it."
  (format nil "ordaprompt: calibration ~a; tau_topic ~a, mu_topic ~a, tau_low ~a~%~
               route an ask: /ordaprompt billing,deploy :: the billing export is empty~%~
               sessions come from the tool: ordaprompt_route~@[~%receipts: ~a~]"
          (if (setting :calibration-active)
              "on (a well-margined match routes on its own)"
              "off (nothing routes automatically; every answer escalates or abstains)")
          (percent (threshold :tau-topic)) (percent (threshold :mu-topic)) (percent (threshold :tau-low))
          (let ((path (setting :receipts-file)))
            (and (stringp path) (plusp (length path)) path))))

(defun run-slash (args session-id)
  "The answer to one /ordaprompt invocation."
  (declare (ignore session-id))
  (let* ((raw (nlk:trimmed (or args "")))
         (split (and (plusp (length raw)) (search "::" raw))))
    (cond
      ((zerop (length raw)) (status-text))
      ((null split) (format nil "usage: /ordaprompt billing,deploy :: the ask   (~a)" (status-text)))
      (t (let ((topics (split-on (subseq raw 0 split) #\,))
               (prompt (nlk:trimmed (subseq raw (+ split 2)))))
           (if (null topics)
               "usage: /ordaprompt billing,deploy :: the ask"
               (describe-result (route prompt topics '()))))))))

;;; --- the manual ----------------------------------------------------------------------------

(defparameter +primer+
  "ordaprompt routes an ask to a kept topic, or to a live session, and refuses to
guess. It scores every candidate in one batch -- topics, a novel-topic
sentinel, an ambiguity sentinel, sessions and a new-session sentinel -- and
answers one band: automatic (a calibrated, well-margined win), escalate (in
between: ask), or abstain_or_new_session (nothing fits, or the session is
contaminated). The automatic band is the section's calibration_active; the
calibration the original shipped locked.

  ordaprompt_route   the tool: prompt, topics, sessions
  /ordaprompt         how it is configured, and how to call it
  /ordaprompt a,b :: ask   one decision over two topic labels

A receipt, when receipts_file names a file, is one JSON line of hashes, labels
and scores: never the prompt, never a file, never a path."
  "What (help :ordaprompt) answers while the cell runs.")

(nle:define-cell ordaprompt
  (:section ("ordaprompt")
    (:guide "nothing here is required; calibration_active unlocks the automatic band, receipts_file keeps a line per decision, and the thresholds are the original's defaults")
    ("calibration_active" :boolean :default t
     :doc "let a calibrated, well-margined match route on its own; off, every answer escalates or abstains")
    ("tau_topic" :integer :default 85 :min 0 :doc "a topic must score this much to route automatically, in hundredths")
    ("mu_topic" :integer :default 20 :min 0 :doc "and beat the second candidate by this much, in hundredths")
    ("tau_novel" :integer :default 85 :min 0 :doc "a novel-topic proposal needs this much residual, in hundredths")
    ("mu_novel" :integer :default 25 :min 0 :doc "and this margin over the best kept topic, in hundredths")
    ("tau_sess_sim" :integer :default 90 :min 0 :doc "a session must be this similar to be reused, in hundredths")
    ("tau_sess_util" :integer :default 55 :min 0 :doc "and carry at least this much utility, in hundredths")
    ("mu_sess" :integer :default 25 :min 0 :doc "and beat the new-session option by this much, in hundredths")
    ("tau_low" :integer :default 60 :min 0 :doc "under this score the router abstains rather than guessing, in hundredths")
    ("k_block" :integer :default 50 :min 0 :doc "contamination at or above this blocks reuse, in hundredths")
    ("w_cost" :integer :default 30 :min 0 :doc "what a session's context costs its utility, in hundredths")
    ("w_contam" :integer :default 50 :min 0 :doc "what a session's contamination risks, in hundredths")
    ("new_session_utility" :integer :default 35 :min 0 :doc "the fixed utility of starting fresh, in hundredths")
    ("context_cost_norm" :integer :default 8000 :min 100
     :doc "tokens at which a session's context cost is counted whole")
    ("receipts_file" :path :doc "a JSONL file of hashes-only receipts; empty keeps none"))
  (:tool "ordaprompt_route" (nlk:contract 'route-arguments) 'route-tool)
  (:command "ordaprompt" #'run-slash
            :description "Route an ask to a kept topic or a live session, or abstain"
            :argument-hint "billing,deploy :: the ask")
  (:help :ordaprompt "ordaprompt: fail-closed routing of an ask to a topic or session - (ordaprompt:route \"ask\" '(\"billing\"))" +primer+))
