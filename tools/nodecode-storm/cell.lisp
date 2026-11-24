;;;; cell.lisp --- one round of a perspective panel: seats, then a judge.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of bmtrnavsky's storm panel (see NOTICE). The host's own side
;;;; call runs every seat on every model the caller names, then one judge call
;;;; synthesizes; nothing is invented and nothing is swapped in for a model
;;;; that fails. Every call goes through NLE:COMPLETE, so the cell holds no
;;;; key and reads no credential store: the provider and model it names ride
;;;; the pin WITH-AUXILIARY-MODEL already resolves, and an empty lineup or
;;;; judge runs on the session's own model.

(in-package #:nodecode-storm)

(defparameter +max-seats+ 12
  "How many seats one round may carry, as the panel spec caps it.")

(defparameter +max-slots+ 4
  "How many models one seat may be run on, as the panel spec caps it.")

(defparameter +manual+
  "A perspective panel: one question, several seats, one judge, one human.
Redraft the question as a tool call, one round per model lineup:
  (storm:panel \"Should we ship on Friday?\"
                '((:name \"operations\" :brief \"the deploy window\"
                   :questions (\"Is the window open?\"))
                  (:name \"support\" :brief \"the on-call queue\"))
                :lineup '(\"alpha/model-a\" \"beta/model-b\") :judge \"gamma/model-c\")
  Topic is the question under review. Each seat is a viewpoint with a name, a brief and the
  questions it must answer from that viewpoint only. Each seat answers on every model in the
  lineup, so two seats on two models is four reports. Lineup and judge are
  \"provider/model\", or a bare model on the session's own provider; empty, absent or nil
  runs on the session's own model, and the storm section's lineup and judge are the
  defaults. Judge is a model, not a seat: it synthesizes and recommends, the human decides.
  Answers one JSON object: {\"success\": true, \"panel\": {\"seat_reports\": [{\"seat\",
  \"model\", \"findings\"}], \"synthesis\", \"contradictions\": [{\"claim\", \"seats_for\",
  \"seats_against\", \"weight\"}], \"unresolved\": [\"...\"]}, \"calls\": [{\"seat\",
  \"model\"}]}. On a failure it is {\"success\": false, \"error\", \"hint\"} and no model
  was swapped in. Caps: 12 seats, 4 lineup slots. The model-facing form is the tool
  storm_run_panel with the same members; /storm shows the lineup.
Use it for a hard call that deserves several viewpoints' evidence rather than one answer."
  "What (help :storm) answers while the cell runs.")

;;; --- reading what a caller passes ---------------------------------------------

(defun str (object key)
  "OBJECT's KEY as a trimmed string, \"\" when it is absent, blank or not a string."
  (let ((value (nlk:json-value object :string key)))
    (if value (nlk:trimmed value) "")))

(defun strings (object key)
  "OBJECT's KEY as its list of strings; NIL when it carries none."
  (let ((array (nlk:json-value object :array key)))
    (when array (loop for item across array when (stringp item) collect item))))

(defun seat-plists (array)
  "The seats in ARRAY, a JSON array of objects, as (:name :brief :questions)
plists; a member of the wrong shape reads empty and the round refuses by name.
A plist written in eval passes through unchanged."
  (loop for item across (or array #())
        collect (if (and (hash-table-p item) (not (eq item :null)))
                    (list :name (str item "name")
                          :brief (str item "brief")
                          :questions (strings item "questions"))
                    item)))

(defun split-target (text)
  "TEXT as (:provider ... :model ...): \"provider/model\" splits at the first
slash, a bare model rides the session's own provider (PROVIDER left out)."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (when (and text (plusp (length text)))
      (let ((slash (position #\/ text)))
        (if slash
            (list :provider (subseq text 0 slash) :model (subseq text (1+ slash)))
            (list :model text))))))

(defun label (target)
  "TARGET as provider/model, or the words for running on the session's own."
  (if (null target)
      "the session's own model"
      (format nil "~@[~a/~]~a" (getf target :provider) (getf target :model))))

(defun resolve-slots (lineup)
  "LINEUP as a list of targets; empty means one slot on the session's own
model. The section's lineup stands in when the call names none."
  (let ((lineup (or lineup (setting :lineup))))
    (if lineup
        (loop for item in lineup for target = (split-target item) when target collect target)
        (list nil))))

(defun resolve-judge (judge)
  "JUDGE as a target, or NIL for the session's own model; the section's
judge stands in when the call names none, and an empty one means the same."
  (flet ((named (text)
           "TEXT as a target, or NIL when it names no model."
           (let ((text (and (stringp text) (nlk:trimmed text))))
             (when (and text (plusp (length text)))
               (let ((target (split-target text)))
                 (if (and target (plusp (length (getf target :model))))
                     target
                     (fail "judge ~s names no model" text)))))))
    (or (named judge) (named (setting :judge)))))

;;; --- the two prompts ----------------------------------------------------------

(defun seat-system (seat)
  "The whole instruction one seat answers under."
  (with-output-to-string (out)
    (format out "You are the ~a seat on a perspective panel.~%Seat brief: ~a~%"
            (getf seat :name) (getf seat :brief))
    (format out "Answer these questions from this seat's viewpoint only:~%")
    (dolist (question (or (getf seat :questions) (list "(none given)")))
      (format out "- ~a~%" question))
    (format out "Ground factual claims in retrievable sources and list them. Mark anything ~
unsourced as unverified. End with 3-5 key findings.")))

(defun judge-system (topic)
  "The judge's instructions: it recommends, the human decides."
  (format nil "You are the panel judge. Recommend, do not decide: the human ~
makes the call.~%Topic: ~a~%~%~
Synthesize: consensus first, then contradictions WITH weights (which seats, ~
what evidence, never averaged away), then what stays unresolved or ~
unverified. Ties and value calls go to the human with the trade stated ~
plainly.~%~%~
Answer as one JSON object with members \"synthesis\" (the prose synthesis), ~
\"contradictions\" (objects with \"claim\", \"seats_for\", \"seats_against\" ~
and \"weight\") and \"unresolved\" (strings)."
          topic))

(defun judge-user (reports)
  "The seat reports as the judge reads them, one block each."
  (with-output-to-string (out)
    (format out "Seat reports:~%")
    (dolist (report reports)
      (format out "~%--- ~a (via ~a) ---~%~a~%"
              (getf report :seat) (getf report :model) (getf report :findings)))))

;;; --- the round ---------------------------------------------------------------

(defun call-model (target system user max-tokens)
  "One side call on TARGET, NIL being the session's own model: the answer's
text. A failure signals and is never retried on another model."
  (let ((nle::*auxiliary-model*
          (when target
            (list :provider (or (getf target :provider) (nle:auxiliary-model))
                  :model (getf target :model)
                  :effort nil))))
    (nle:complete system user :max-tokens max-tokens :fallback-p nil)))

(defun hint-for (condition)
  "Plain words on what to try next; the run stopped, nothing was swapped in."
  (let ((text (string-downcase (princ-to-string condition))))
    (cond ((or (search "not running" text) (search "credential" text)
               (search "no provider" text) (search "auth" text))
           "Check the model the panel names is reachable from this session: /models lists the lanes, and the storm section's lineup and judge set the defaults; an empty one runs the session's own model.")
          (t "Check that every provider and model in the lineup is reachable right now. The run stopped; nothing was swapped in for the failed model."))))

(defun failure-json (problem hint)
  "The round's answer when it could not finish: what happened, and a next step."
  (nlk:encode-json-object
   (nlk:json-object "success" nil "error" problem "hint" hint)))

(defun judged-object (text)
  "TEXT as the judge's JSON object; one that wrote prose instead keeps its
words as the synthesis."
  (let* ((start (and (stringp text) (position #\{ text)))
         (end (and start (position #\} text :from-end t)))
         (parsed (and end (ignore-errors (nlk:decode-json (subseq text start (1+ end)))))))
    (if (hash-table-p parsed)
        parsed
        (nlk:json-object "synthesis" (or text "") "contradictions" #() "unresolved" #()))))

(defun panel-json (reports judged calls)
  "One round's answer: the reports, the judge's object, and which seat ran on
which model."
  (nlk:encode-json-object
   (nlk:json-object
    "success" t
    "panel"
    (nlk:json-object
     "seat_reports"
     (coerce (loop for report in reports
                   collect (nlk:json-object "seat" (getf report :seat)
                                             "model" (getf report :model)
                                             "findings" (getf report :findings)))
             'vector)
     "synthesis" (or (nlk:json-value judged :string "synthesis") "")
     "contradictions" (or (nlk:json-value judged :array "contradictions") #())
     "unresolved" (or (nlk:json-value judged :array "unresolved") #()))
    "calls"
    (coerce (loop for call in calls
                  collect (nlk:json-object "seat" (getf call :seat)
                                            "model" (getf call :model)))
            'vector))))

(defun panel (topic seats &key lineup judge max-tokens)
  "One perspective-panel round, the verb (help :storm) documents. TOPIC is
the question under review, SEATS a list of (:name :brief :questions) plists,
LINEUP a list of \"provider/model\" targets, JUDGE one target. Answers one
JSON string; a failure answers success false with a hint and swaps nothing."
  (running-settings)
  (let ((topic (if (stringp topic) (nlk:trimmed topic) ""))
        (seats (if (and (vectorp seats) (not (stringp seats))) (seat-plists seats) seats))
        (max-tokens (or max-tokens (setting :max-tokens))))
    (cond
      ((zerop (length topic)) (failure-json "topic must be a non-empty string" (hint-for "topic")))
      ((null seats) (failure-json "seats must carry at least one seat card" (hint-for "seats")))
      ((> (length seats) +max-seats+)
       (failure-json (format nil "~d seats is more than the ~d a round carries" (length seats) +max-seats+)
                     (hint-for "seats")))
      (t
       (let ((missing (loop for seat in seats
                            unless (and (stringp (getf seat :name)) (plusp (length (getf seat :name)))
                                        (stringp (getf seat :brief)) (plusp (length (getf seat :brief))))
                              collect (or (getf seat :name) seat))))
         (if missing
             (failure-json (format nil "every seat needs a name and a brief: ~s" missing)
                           (hint-for "seat"))
             (let ((slots (resolve-slots lineup)))
               (if (> (length slots) +max-slots+)
                   (failure-json (format nil "~d lineup slots is more than the ~d a seat carries"
                                          (length slots) +max-slots+)
                                 (hint-for "lineup"))
                   (handler-case
                       (let ((reports '()) (calls '()))
                         (dolist (seat seats)
                           (dolist (slot slots)
                             (let ((answer (call-model slot (seat-system seat)
                                                       (format nil "Topic: ~a" topic) max-tokens)))
                               (push (list :seat (getf seat :name) :model (label slot)
                                           :findings answer)
                                     reports)
                               (push (list :seat (getf seat :name) :model (label slot)) calls))))
                         (setf reports (nreverse reports) calls (nreverse calls))
                         (let ((judge-target (resolve-judge judge)))
                           (panel-json
                            reports
                            (judged-object
                             (call-model judge-target
                                         (judge-system topic) (judge-user reports) max-tokens))
                            calls)))
                     (storm-error (condition)
                       (failure-json (princ-to-string condition) (hint-for condition)))
                     (error (condition)
                       (failure-json (princ-to-string condition) (hint-for condition))))))))))))

;;; --- the tool the model calls ---------------------------------------------------

(defparameter +schema+
  (list :description
        "Run one perspective-panel round: every seat answers on every model in the lineup, then one judge synthesizes. Pass the seats and the lineup explicitly; nothing is invented, and a failed model is never swapped for another. Answers JSON: seat reports, synthesis, weighted contradictions, what stays unresolved."
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "topic" (nlk:make-json-object
                   "type" "string"
                   "description" "The question under review.")
          "seats" (nlk:make-json-object
                   "type" "array"
                   "description" "Seat cards: a viewpoint's name, its brief, the questions it must answer. At most 12."
                   "items" (nlk:make-json-object
                            "type" "object"
                            "properties"
                            (nlk:make-json-object
                             "name" (nlk:make-json-object "type" "string"
                                                           "description" "The seat's name, e.g. \"operations\".")
                             "brief" (nlk:make-json-object "type" "string"
                                                            "description" "What this seat sees that the others do not.")
                             "questions" (nlk:make-json-object
                                          "type" "array"
                                          "items" (nlk:make-json-object "type" "string")
                                          "description" "The questions this seat must answer from its viewpoint only."))
                            "required" (vector "name" "brief")))
          "lineup" (nlk:make-json-object
                    "type" "array"
                    "items" (nlk:make-json-object "type" "string")
                    "description" "Models to run every seat on, as \"provider/model\" or a bare model on the session's provider. At most 4; empty or absent runs the session's own model.")
          "judge" (nlk:make-json-object
                   "type" "string"
                   "description" "The judge's model as \"provider/model\". Empty or absent judges on the session's own model.")
          "max_tokens" (nlk:make-json-object
                         "type" "integer"
                         "description" "One side call's completion ceiling; absent means the storm section's max_tokens."))
         "required" (vector "topic" "seats")
         "additionalProperties" nil))
  "What the provider is shown for storm_run_panel, and what a call is held to.")

(defun run-tool (arguments)
  "The tool body: the decoded arguments as a table, the round's answer as text."
  (handler-case
      (panel (str arguments "topic")
             (seat-plists (nlk:json-value arguments :array "seats"))
             :lineup (strings arguments "lineup")
             :judge (str arguments "judge")
             :max-tokens (nlk:json-value arguments :integer "max_tokens"))
    (storm-error (condition) (failure-json (princ-to-string condition) (hint-for condition)))
    (error (condition) (failure-json (princ-to-string condition) (hint-for condition)))))

;;; --- /storm --------------------------------------------------------------------

(defun status ()
  "The lineup, the judge and the ceiling, in one line; no network."
  (running-settings)
  (let ((lineup (setting :lineup))
        (judge (setting :judge)))
    (format nil "storm: seats on ~{~a~^, ~}; judge on ~a; max_tokens ~d~%a round runs through the storm_run_panel tool; (help :storm) has the method"
            (if lineup (mapcar (lambda (target) (label (split-target target))) lineup)
                (list (label nil)))
            (if (and judge (plusp (length (nlk:trimmed judge)))) (label (split-target judge)) (label nil))
            (setting :max-tokens))))

(defun panel-dialog (summary)
  "The dialog bare /storm answers: SUMMARY, over the row that re-reads it."
  (nle:list-dialog
   :title "Storm panel" :summary summary
   :rows (list (list :label "Show the lineup" :run (list :enter "/storm status")
                     :detail "what the next round runs on"))))

(defun run-slash (args session-id)
  "/storm typed alone: the lineup and how a round is asked for; the row runs
/storm status, which asks no model."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((equal verb "") (let ((text (status))) (values text (panel-dialog text))))
          ((equal verb "status") (status))
          (t (values (format nil "storm: ~a is not one of its choices (status)" verb)
                     (panel-dialog (status)))))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell storm
  (:section ("storm")
    (:guide "nothing is required; an empty lineup runs every seat on the session's own model, empty judge judges on it too; max_tokens bounds each side call; the section's lineup and judge are the defaults the tool's own arguments override")
    ("lineup" :list :doc "models to run every seat on, as provider/model or a bare model; empty runs the session's own")
    ("judge" :string :default "" :doc "the judge's model as provider/model; empty judges on the session's own")
    ("max_tokens" :integer :default 4000 :min 256 :doc "one side call's completion ceiling"))
  (:tool "storm_run_panel" +schema+ 'run-tool)
  (:help :storm "storm panel: one question across several model seats, then a judge - (storm:panel \"topic\" '((:name \"operations\" :brief \"...\")))" +manual+)
  (:command "storm" 'run-slash
            :description "The perspective panel's lineup, and what a round runs on"))
