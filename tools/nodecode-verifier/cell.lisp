;;;; cell.lisp --- the verifier: a review per turn, and a veto before a risky call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of am423/hermes-verifier's runtime.py, delta.py, brief.py,
;;;; gate.py and guard.py. What the Hermes plugin did through post_llm_call,
;;;; pre_llm_call, pre_tool_call and pre_verify hooks, a Nodecode cell does
;;;; through two points: a turn that reaches a terminal fact is queued on
;;;; :FRAME and reviewed in the background, the finding it raises stands as a
;;;; notice the agent reads in its next request; and an impending tool call
;;;; that reads destructive or publishing is put to the model on :TOOL, which
;;;; may refuse it. The gate fails open: a gate that cannot decide allows.
;;;;
;;;; Config, a sibling top-level key next to `channels':
;;;;   "verifier": {"enabled": true, "phase_gate": true, "agreement": 1}

(in-package #:nodecode-verifier)

(nlk:define-peripheral verifier :not-running t)

(defparameter +primer+
  "The verifier is a second model that reviews this session's work. Set
`verifier: {\"enabled\": true}' and every completed turn is reviewed in the
background: the finding it raises stands as a notice under this cell's key
and rides your next request. Or ask for one by hand, with (help :verifier).

  /verify                on or off, the model, the gate, the last finding
  /verify on | off       review every completed turn, or stop
  /verify gate on | off  let a review veto a destructive or publishing call
  /verify review [focus] review the newest turn now
  /verify agree N M      N reviews a turn, M of them to deliver a finding
  /verify dump           the last finding, with its severity
  /verify reset          drop the last finding and the standing notice
  /verify test SEV NOTE  deliver a test finding: note, caution or blocker

A review raises at most one finding, of three severities: note (cleanup),
caution (a material risk) or blocker (continuing wastes the work). Silence is
\"nothing to flag\". With the phase gate on, a call about to delete, force
push, restart or publish is shown to the model first; it answers `allow` or
`block: why`, and any failure to answer allows the call. Review priorities of
a project's own are read from a VERIFIER.md file found from the working
directory up, and never added to your own instructions."
  "What (help :verifier) answers while the cell runs.")

;;; --- the cell's own state ----------------------------------------------------

(defvar *enabled* nil
  "Whether completed turns are reviewed: the section's `enabled' to begin
with, and /verify on or off turns it either way without a restart.")

(defvar *pending* '()
  "Terminal turns waiting to be reviewed, newest first: (SESSION TURN).")

(defvar *worker* nil
  "The reviewing thread while it runs, or NIL.")

(defvar *workers* t
  "Whether START-CELL runs the reviewing thread; a test calls TICK by hand.")

(defvar *last* (make-hash-table :test #'equal)
  "The last finding reviewed per session, for /verify dump.")

(defvar *phase-checks* 0
  "Phase-gate calls made in this run, for /verify status.")

(defvar *gate* nil
  "Whether the phase gate is on: the section's `phase_gate' to begin with,
and /verify gate on or off turns it either way without a restart.")

(defvar *agreement* nil
  "The rounds /verify agree set for this run; the section's when NIL.")

(defvar *agreement-min* nil
  "The agreeing reviews /verify agree set for this run; the section's when NIL.")

;;; --- the verifier's instruction ----------------------------------------------

(defparameter +system-prompt+
  "You are the Verifier for a coding session. You review what the primary
agent just did and raise a finding that steers it before it goes wrong.

You are a reviewer, not an executor. You never run tools, never edit files and
never take actions of your own; you read the transcript you are given and
answer.

Rules:
- At most ONE finding per review. If nothing matters, answer exactly:
  nothing to flag
- A finding is one line, prefixed by its severity and a colon:
    note: ...     small cleanup, simplification, a low-risk edge case
    caution: ...  a material risk: wrong direction, a missing constraint,
                  an API that does not exist, work about to be wasted
    blocker: ...  continuing will clearly waste the work or ship it broken
- Be concrete: name the exact risk, the exact tool call or claim, and what to
  do instead. Vague praise is not a finding.
- The transcript has been redacted; you never see secrets and must not ask
  for them. Review only what is new since your last review.
- Keep the finding under 200 words. When the work is sound, say nothing."
  "The reviewer's whole instruction, as the original plugin wrote it.")

(defparameter +judge-prompt+
  "You are the phase gate of a coding session's verifier. One tool call is
about to run and it reads destructive or publishing. Decide whether it should.

Answer exactly one line, nothing else:
  allow
or
  block: <one line naming what would be lost, and what to do instead>

Block only when the call is irreversible AND wrong or unverified: a delete of
something not named by the user, a force push over unpushed work, a publish or
deploy nothing asked for, a restart of a service the task does not concern.
When in doubt, allow; the agent is the executor."
  "The phase gate's instruction.")

;;; --- redaction ---------------------------------------------------------------

(defparameter +secret-markers+ '("sk-" "ghp_" "github_pat_" "gho_" "xoxb-" "xoxp-"
                                 "AKIA" "ASIA" "AIza" "eyJ")
  "Markers that make a word a credential: the prefixes the original redacted.")

(defparameter +secret-labels+ '("key" "token" "secret" "password" "passwd")
  "A word naming one of these, ending in `=' or `:', makes the next word a secret.")

(defun marker-word-p (word)
  (some (lambda (marker) (search marker word)) +secret-markers+))

(defun label-word-p (word)
  (let ((lowered (string-downcase word)))
    (and (plusp (length lowered))
         (member (char lowered (1- (length lowered))) '(#\= #\:))
         (some (lambda (label) (search label lowered)) +secret-labels+))))

(defun pem-scrub (text)
  "TEXT with a private key block cut out whole, or TEXT unchanged."
  (let ((start (search "-----BEGIN" text)))
    (if (null start)
        text
        (let* ((end (search "-----END" text :start2 (+ start 10)))
               (close (and end (search "-----" text :start2 (+ end 8))))
               (stop (and close (+ close 5))))
          (if stop
              (concatenate 'string (subseq text 0 start) "[redacted]"
                           (pem-scrub (subseq text stop)))
              text)))))

(defun redact (text)
  "TEXT as the verifier may see it: a credential after a key-ish label, or a
word carrying a token prefix, is replaced by [redacted]."
  (let ((text (pem-scrub (or (and (stringp text) text) ""))))
    (with-output-to-string (out)
      (let ((word '()) (previous ""))
        (labels ((flush ()
                   (when word
                     (let ((text (coerce (nreverse word) 'string)))
                       (write-string (if (or (marker-word-p text)
                                             (and (label-word-p previous) (plusp (length text))))
                                         "[redacted]" text)
                                     out)
                       (setf previous text word '())))))
          (loop for ch across text
                do (if (member ch '(#\Space #\Tab #\Newline #\Return))
                       (progn (flush) (write-char ch out))
                       (push ch word)))
          (flush))))))

;;; --- what a review may say ----------------------------------------------------

(defparameter +severities+ '(("note" . 1) ("caution" . 2) ("blocker" . 3)))

(defun setting-value (key fallback)
  "SETTING KEY while the cell runs, else FALLBACK: a helper a test may call
with nothing started."
  (or (ignore-errors (setting key)) fallback))

(defun clamp-text (text limit)
  (let ((text (if (stringp text) text "")))
    (if (and limit (> (length text) limit))
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

(defun severity-rank (severity)
  (or (cdr (assoc severity +severities+ :test #'string=)) 1))

(defun normalize-finding (text)
  "The key two reviews raising the same point share: case folded, whitespace
squeezed, trailing punctuation dropped."
  (string-downcase
   (string-trim " .,;:!?'\""
                (with-output-to-string (out)
                  (let ((space nil) (any nil))
                    (loop for ch across (or text "") do
                      (if (member ch '(#\Space #\Tab #\Newline #\Return))
                          (setf space t)
                          (progn (when (and space any) (write-char #\Space out))
                                 (setf space nil any t)
                                 (write-char ch out)))))))))

(defun parse-finding (text max-chars)
  "=> (:severity S :finding F) for one review's TEXT, or NIL for silence."
  (let ((text (and (stringp text)
                   (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (when (and text (plusp (length text)))
      (let ((lowered (string-downcase text)) (found nil))
        (dolist (pair +severities+)
          (let ((at (search (car pair) lowered)))
            (when (and at (or (null found) (< at (first found))))
              (setf found (list at (car pair))))))
        (when found
          (let* ((at (first found))
                 (severity (second found))
                 (colon (position #\: text :start (+ at (length severity))))
                 (body (string-trim '(#\Space #\Tab #\: #\[ #\])
                                    (if colon (subseq text (1+ colon)) text))))
            (when (and (plusp (length body))
                       (null (search "nothing to flag" (string-downcase body))))
              (list :severity severity :finding (clamp-text body max-chars)))))))))

(defun agreeing (answers max-chars min-count)
  "The one finding the reviews agree on: => (values FINDING COUNT), NIL when
fewer than MIN-COUNT of them raised the same point."
  (let ((findings (remove nil (mapcar (lambda (answer) (parse-finding answer max-chars))
                                      answers))))
    (when findings
      (let ((groups '()))
        (dolist (finding findings)
          (let ((key (normalize-finding (getf finding :finding)))
                (cell (assoc (normalize-finding (getf finding :finding)) groups
                             :test #'string=)))
            (if cell
                (push finding (cdr cell))
                (push (list key finding) groups))))
        (let ((winners (remove-if (lambda (group) (< (length (cdr group)) min-count)) groups)))
          (when winners
            (let* ((group (first (sort winners #'> :key (lambda (g) (length (cdr g))))))
                   (best (first (sort (copy-list (cdr group)) #'>
                                      :key (lambda (f) (severity-rank (getf f :severity)))))))
              (values best (length (cdr group))))))))))

(defun finding-line (finding)
  (format nil "[~:@(~a~)] ~a" (getf finding :severity) (getf finding :finding)))

;;; --- review priorities of the project's own -------------------------------------

(defparameter +priorities-file+ "VERIFIER.md"
  "The file a project keeps its review priorities in, as the original did.")

(defun read-capped (path)
  "PATH's text, at most 16 KiB, or NIL."
  (when (and (stringp path) (plusp (length path)) (probe-file path))
    (ignore-errors
     (with-open-file (in path :direction :input :if-does-not-exist nil)
       (when in
         (let ((text (make-string 16384)))
           (let ((n (read-sequence text in)))
             (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq text 0 n)))))))))

(defun priorities-text ()
  "The section's priorities file, else VERIFIER.md found from the working
directory upward: review guidance that never reaches the agent's own prompt."
  (let ((named (setting-value :priorities "")))
    (or (and (stringp named) (plusp (length named)) (read-capped named))
        (loop with directory = (ignore-errors (uiop:getcwd))
              repeat 8
              while directory
              for text = (read-capped (merge-pathnames +priorities-file+ directory))
              when text return text
              do (setf directory (ignore-errors (uiop:pathname-parent-directory-pathname directory)))))))

(defun review-system-prompt ()
  (let ((priorities (priorities-text)))
    (if priorities
        (format nil "~a~%~%Especially pay attention to:~%<attention>~%~a~%~a"
                +system-prompt+ priorities "</attention>")
        +system-prompt+)))

;;; --- one review ---------------------------------------------------------------

(defvar *reviewer* nil
  "A function of (SYSTEM USER) answering a review, which a test binds; NIL
means the real side call.")

(defvar *judge* nil
  "A function of (SYSTEM USER) answering the phase gate, which a test binds.")

(defvar *model-override* ""
  "The model /verify model set for this run; the section's when empty.")

(defvar *provider-override* ""
  "The provider /verify provider set for this run; the section's when empty.")

(defun chosen (override key)
  (if (and (stringp override) (plusp (length override)))
      override
      (setting-value key "")))

(defun ask-model (system user max-tokens)
  "One side call: the cell's own provider/model when its section names one,
else the auxiliary target the organism already selects for side work."
  (let* ((provider (chosen *provider-override* :provider))
         (model (chosen *model-override* :model))
         (override (or (plusp (length provider)) (plusp (length model)))))
    (if override
        (multiple-value-bind (default-provider default-model)
            (nle:auxiliary-model)
          (let ((nle::*auxiliary-model*
                  (list :provider (if (plusp (length provider)) provider default-provider)
                        :model (if (plusp (length model)) model default-model))))
            (nle:complete system user :max-tokens max-tokens)))
        (nle:complete system user :max-tokens max-tokens))))

(defun deliver (text)
  "Stand a finding under this cell's key: every shell shows it, and the agent
reads it in the notices block of its next request."
  (nle:notice text :level :info :key "nodecode-verifier"))

(defun transcript-from-log (session &key (limit 60))
  "The newest LIMIT messages of SESSION as a markdown transcript: who said
what, a tool call's arguments and a tool result's text, each clipped."
  (let ((clip (setting-value :clip 500))
        (rows '()))
    (ignore-errors
     (setf rows (nlk:recall-log :session-id session :limit limit :clip clip)))
    (when (and rows (getf (first rows) :matched))
      (pop rows))
    (format nil "~{~a~^~%~%~}"
            (loop for row in rows
                  for who = (or (getf row :role) (getf row :tool))
                  for body = (getf row :text)
                  when (and who body (plusp (length body)))
                    collect (format nil "#### ~a~%~a" who (clamp-text body clip))))))

(defun review (transcript &key session focus)
  "Review TRANSCRIPT on the verifier model. => (values TEXT FINDING COUNT):
TEXT what the agent reads (\"nothing to flag\" when there is nothing),
FINDING the accepted one or NIL, COUNT how many reviews raised it."
  (let* ((session (or session nlk:*scribe-session-id* "default"))
         (max-chars (setting-value :max-finding-chars 1200))
         (rounds (max 1 (or *agreement* (setting-value :agreement 1) 1)))
         (min-count (min rounds (max 1 (or *agreement-min* (setting-value :agreement-min 1) 1))))
         (system (review-system-prompt))
         (user (format nil "~@[#### What to watch for~%~a~%~%~]~a"
                       (and focus (plusp (length focus)) focus)
                       (redact (or transcript ""))))
         (answers (loop repeat rounds
                         collect (if *reviewer*
                                     (funcall *reviewer* system user)
                                     (ignore-errors (ask-model system user 900))))))
    (multiple-value-bind (finding count) (agreeing answers max-chars min-count)
      (setf (gethash session *last*) finding)
      (if finding
          (progn (deliver (format nil "Verifier review~%~%~a" (finding-line finding)))
                 (values (finding-line finding) finding count))
          (progn (deliver nil)
                 (values "nothing to flag" nil 0))))))

(defun review-turn (session &key focus turn-id)
  "Review the newest turn of SESSION and deliver what it found.
=> a line saying what happened."
  (declare (ignore turn-id))
  (let ((transcript (transcript-from-log session)))
    (if (or (null transcript) (zerop (length (string-trim " " transcript))))
        "no turn to review yet"
        (multiple-value-bind (text finding count) (review transcript :session session :focus focus)
          (if finding
              (format nil "reviewed ~a: ~a" session (finding-line finding))
              (format nil "reviewed ~a: ~a~@[ (~d of them raised it)~]" session text
                      (and (plusp count) count)))))))

(defun tick ()
  "Drain the queue: one review per terminal turn, newest first, and never an
error out of the worker."
  (loop while *pending*
        do (let ((entry (pop *pending*)))
             (ignore-errors (review-turn (first entry) :turn-id (second entry))))))

(defun queue-turn (session turn)
  (setf *pending* (cons (list session turn)
                        (remove session *pending* :key #'first :test #'string=)))
  (when *worker* (ignore-errors (nlk:worker-poke *worker*))))

(defun observe-frame (op)
  "A :FRAME observer: a turn that reached a terminal fact is queued for review
when the cell is on. Publishing thread: a list and a poke, no I/O."
  (let ((session (getf op :session-id)))
    (when (and *enabled* (stringp session))
      (multiple-value-bind (type payload metadata turn-id) (nlk:frame-fact op)
        (declare (ignore payload metadata))
        (when (and (stringp type)
                   (member type '("turn.completed" "turn.failed" "turn.cancelled")
                           :test #'string=))
          (queue-turn session (or turn-id "")))))))

;;; --- the phase gate -------------------------------------------------------------

(defparameter +risky-patterns+
  '("rm -rf" "rm -fr" "rm -r " "drop table" "drop database" "truncate table"
    "push --force" "push -f " "force-push" "reset --hard" "mkfs" "dd if=" "chmod 000"
    "shutdown" "reboot" "systemctl restart" "npm publish" "cargo publish" "docker push"
    "kubectl delete" "terraform apply" "deploy")
  "What makes an impending call worth a look: a phrase, not a shell parser. A
false positive costs one small model call and an allow.")

(defun arguments-text (args)
  "The text of ARGS a pattern can read: every string it carries."
  (cond ((hash-table-p args)
         (format nil "~{~a~^ ~}"
                 (loop for value being the hash-values of args
                       when (stringp value) collect value)))
        ((stringp args) args)
        (t (format nil "~s" args))))

(defun risky-label (name text)
  "The phrase that makes this call worth the gate's look, or NIL."
  (let ((blob (string-downcase (format nil "~a ~a" (or name "") (or text "")))))
    (find-if (lambda (pattern) (search pattern blob)) +risky-patterns+)))

(defun parse-verdict (text)
  "The gate's answer: => (values :BLOCK REASON) or (values :ALLOW NIL)."
  (let ((text (and (stringp text)
                   (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (if (and text (search "block" (string-downcase text)))
        (values :block (clamp-text text 300))
        (values :allow nil))))

(defun judge-call (name text)
  "One small model call about one impending call; any failure to decide allows."
  (let ((answer (if *judge*
                    (funcall *judge* +judge-prompt+
                             (format nil "The call about to run:~%~%~a ~a"
                                     name (clamp-text text 2000)))
                    (ignore-errors
                     (ask-model +judge-prompt+
                                (format nil "The call about to run:~%~%~a ~a"
                                        name (clamp-text text 2000))
                                200)))))
    (incf *phase-checks*)
    answer))

(defun gate-hook (op next)
  "The :TOOL point: an impending destructive or publishing call is shown to
the verifier, whose block refuses it. Off by default, and it fails open."
  (let* ((name (getf op :name))
         (text (and (hash-table-p (getf op :arguments))
                    (arguments-text (getf op :arguments))))
         (label (and *enabled* *gate* (risky-label name text))))
    (if (null label)
        (funcall next op)
        (multiple-value-bind (action reason)
            (handler-case (parse-verdict (judge-call name text))
              (error () (values :allow nil)))
          (if (eq action :block)
              (nle:failure
               (format nil "ERROR: held by nodecode-verifier (phase gate: ~a)~%~a~%-- address it, or say why it is safe and call again"
                       label (or reason "")))
              (funcall next op))))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract verifier-review-arguments
  (:doc "Have a second model verify work before you build on it. Pass the transcript: the user's ask, what you did, every tool call and result, your answer. It answers one finding tagged [note], [caution] or [blocker], or \"nothing to flag\". It reviews; it never edits files or runs anything. Use it when the work is substantial, a refactor is spreading, or you are unsure the answer actually does what was asked.")
  ("transcript" :text :required t
                :doc "the work to verify, as text: the request, the steps, the tool calls and results, the answer")
  ("focus" :string :doc "one line on what to watch for, when it is narrower than the transcript"))

(defun review-tool (args)
  "The verifier_review tool: one review of what the model passed, its finding
as the answer."
  (multiple-value-bind (text finding count)
      (review (verifier-review-arguments-transcript args)
              :focus (verifier-review-arguments-focus args))
    (if finding
        (format nil "~a~@[ (~d of ~d reviews raised it)~]" text count
                (max 1 (or (setting-value :agreement 1) 1)))
        text)))

;;; --- /verify -------------------------------------------------------------------

(defun named-or (value fallback)
  (if (and (stringp value) (plusp (length value))) value fallback))

(defun status-line (session)
  (let ((last (gethash (or session "default") *last*)))
    (format nil "verifier ~a - model ~a, provider ~a, phase gate ~a, agreement ~d of ~d,~%~
last finding: ~a. ~d gate call~:p this run~%~
Usage: /verify [on|off|gate|review|agree|dump|reset|test]"
            (if *enabled* "on (every turn)" "off")
            (named-or (chosen *model-override* :model) "(auxiliary model)")
            (named-or (chosen *provider-override* :provider) "(auxiliary)")
            (if *gate* "on" "off")
            (max 1 (or *agreement-min* (setting-value :agreement-min 1) 1))
            (max 1 (or *agreement* (setting-value :agreement 1) 1))
            (if last (finding-line last) "none yet")
            *phase-checks*)))

(defun split-words (text)
  (let ((words '()) (word '()))
    (flet ((flush () (when word (push (coerce (nreverse word) 'string) words) (setf word '()))))
      (loop for ch across (or text "")
            do (if (member ch '(#\Space #\Tab #\Newline))
                   (flush)
                   (push ch word)))
      (flush))
    (nreverse words)))

(defun run-slash (args session-id)
  "The answer to one /verify invocation."
  (let* ((raw (nlk:trimmed (or args "")))
         (space (position #\Space raw))
         (head (string-downcase (if space (subseq raw 0 space) raw)))
         (value (if space (string-trim " " (subseq raw (1+ space))) "")))
    (nlk:dispatch head string=
      ("" (status-line session-id))
      ("status" (status-line session-id))
      ("on" (setf *enabled* t)
       "verifier on: every completed turn is reviewed, and a finding stands in the notices")
      ("off" (setf *enabled* nil)
       "verifier off: only /verify review and the verifier_review tool review anything")
      ("gate" (let ((word (string-downcase value)))
                (cond ((string= word "on")
                       (setf *gate* t)
                       "phase gate on: a destructive or publishing call is reviewed before it runs")
                      ((string= word "off")
                       (setf *gate* nil)
                       "phase gate off: no call is held")
                      (t "usage: /verify gate on|off"))))
      ("review" (review-turn session-id :focus value))
      ("agree" (let ((words (split-words value)))
                 (if (and (= 2 (length words))
                          (ignore-errors (parse-integer (first words)))
                          (ignore-errors (parse-integer (second words))))
                     (let* ((rounds (max 1 (parse-integer (first words))))
                            (needed (min rounds (max 1 (parse-integer (second words))))))
                       (setf *agreement* rounds
                             *agreement-min* needed)
                       (format nil "agreement ~d of ~d (this run; set verifier.agreement and verifier.agreement_min to keep it)"
                               needed rounds))
                     "usage: /verify agree ROUNDS NEEDED")))
      ("model" (if (plusp (length value))
                    (progn (setf *model-override* value)
                           (format nil "verifier model ~a (this run; set verifier.model to keep it)" value))
                    (format nil "verifier model ~a" (chosen *model-override* :model))))
      ("provider" (if (plusp (length value))
                      (progn (setf *provider-override* value)
                             (format nil "verifier provider ~a (this run; set verifier.provider to keep it)" value))
                      (format nil "verifier provider ~a" (chosen *provider-override* :provider))))
      ("dump" (let ((last (gethash (or session-id "default") *last*)))
                (if last (finding-line last) "no finding yet")))
      ("reset" (setf (gethash (or session-id "default") *last*) nil)
       (deliver nil)
       "finding dropped and the standing notice cleared")
      ("test" (let* ((words (split-words value))
                     (severity (string-downcase (or (first words) ""))))
                (if (and (member severity '("note" "caution" "blocker") :test #'string=)
                         (rest words))
                    (let ((finding (list :severity severity
                                         :finding (format nil "~{~a~^ ~}" (rest words)))))
                      (setf (gethash (or session-id "default") *last*) finding)
                      (deliver (format nil "Verifier review~%~%~a" (finding-line finding)))
                      (format nil "delivered a test ~a" severity))
                    "usage: /verify test note|caution|blocker <finding>")))
      (t (format nil "~a is not one of its choices; /verify alone shows them" head)))))

;;; --- the entry ------------------------------------------------------------------

(defun install ()
  (setf *enabled* (not (null (setting-value :enabled nil)))
        *gate* (not (null (setting-value :phase-gate nil)))
        *pending* '()
        *phase-checks* 0
        *agreement* nil *agreement-min* nil)
  (clrhash *last*)
  (nle:on-stop (lambda ()
                 (setf *enabled* nil *gate* nil *pending* '()
                       *agreement* nil *agreement-min* nil
                       *model-override* "" *provider-override* "")))
  (when *workers*
    (setf *worker* (nlk:worker-start "nodecode-verifier" #'tick :wake 30))
    (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*))))))

(nle:define-cell verifier
  (:section ("verifier")
    (:guide "enabled is the whole switch: on, every completed turn is reviewed; phase_gate lets a review veto a destructive or publishing call before it runs; agreement runs several reviews and delivers only what they agree on")
    ("enabled" :boolean :default nil :doc "review every completed turn")
    ("model" :string :doc "the verifier's model; empty uses the auxiliary model")
    ("provider" :string :doc "the verifier's provider, beside model")
    ("clip" :integer :default 500 :min 50 :doc "characters kept per tool call or result")
    ("max_finding_chars" :integer :default 1200 :min 100 :doc "the longest finding kept")
    ("agreement" :integer :default 1 :min 1 :doc "reviews run per turn; 1 is off")
    ("agreement_min" :integer :default 1 :min 1 :doc "how many of them must raise the same finding")
    ("phase_gate" :boolean :default nil :doc "review a destructive or publishing call before it runs, and refuse it when the verifier says block; fails open")
    ("priorities" :path :doc "a file of review priorities; empty looks for VERIFIER.md from the working directory up"))
  (:start #'install)
  (:help :verifier "a second model reviews each turn and posts one finding; the phase gate can hold a destructive call before it runs" +primer+)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-verifier"))
  (:hook :tool #'gate-hook)
  (:tool "verifier_review" (nlk:contract 'verifier-review-arguments) 'review-tool)
  (:command "verify" #'run-slash
            :description "A second model verifies the work; /verify on to review every turn"
            :argument-hint "on|off|gate|review|agree|dump|reset|test"))
