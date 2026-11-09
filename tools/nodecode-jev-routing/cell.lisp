;;;; cell.lisp --- the manual, /jev, and the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original routes every turn of a Hermes session by rewriting the
;;;; provider request it is given. A Nodecode cell cannot rewrite a turn, so
;;;; this one is the decision engine the turn would have used: a model calls
;;;; (jev:route TEXT) through eval and acts on the answer -- a subagent on
;;;; that model, a switch with /model, or simply the knowledge of the cost.
;;;; (help :jev) answers the manual while the cell runs.

(in-package #:nodecode-jev-routing)

(defparameter +manual+
  "Jev routing is available through the nodecode-jev-routing cell: per-prompt model routing
from TypeSafe Jev, called through eval. It reads the routing table you point config_path at and
asks System One four typed questions (POST /v1/systemone), then composes a capability tier and
names the model your own table holds for it. It advises; it never changes the model a turn runs on.
  (jev:route PROMPT &key history active-model context-tokens)
      => (:tier \"high\" :desired-tier \"high\" :provider \"openai-codex\" :model \"gpt-6-sol\"
          :thinking \"medium\" :kind \"implement\" :demand 2.31 :confidence 0.81
          :budget-pressure 0.0 :low-confidence nil :downgraded nil :notes (\"...\")
          :glyph \".\" :line \"tier high · openai-codex/gpt-6-sol · kind implement · ...\"
          :latency-ms 120)
      HISTORY is the recent conversation (at most 4000 characters go), ACTIVE-MODEL what the
      session is on now, CONTEXT-TOKENS an estimate of the context in use. (:skipped \"acknowledgement\")
      when the prompt does not re-route: empty, a slash command, an acknowledgement, or a short
      continuation with HISTORY behind it.
  (jev:judge PROMPT &key history active-model context-tokens)
      the four answers alone, for the why: (:kind ... :probabilities (...) :confidence C
      :complexity 2.40 :capability 2.20 :reasoning 0.80 :latency-ms 120 ...).
  (jev:spend)   what the ledger beside the routing table holds: today, this month, the caps.
  (jev:record MODEL &key provider input output usd)
      record one provider response so the budget policy sees it.
The routing table is JSON: routes (quick, standard, high, premium, xpremium, each a list of
{provider, model, thinkingLevel}), kindModels, kindMinimumTier, confidenceThreshold,
minPromptChars, budget {dailyUsd, monthlyUsd, softRatio, hardRatio}, free {enabled, policy,
pool}, pricing {\"provider/model\": {input, output}}. What leaves the machine is the prompt, a
short excerpt, the working directory, the active model, the context estimate and the spend
snapshot, to TypeSafe alone. A missing key, a timeout or a bad answer signals JEV-ERROR and
changes nothing. Use it before a big or a routine piece of work; the ledger only fills for
responses you record."
  "What (help :jev) answers while the cell runs.")

;;; --- the picture --------------------------------------------------------------------

(defun chains-line (router)
  "Every non-empty tier and the head of its chain, in one line."
  (let ((parts (loop for tier across +tiers+
                     for chain = (route-of router tier)
                     when chain
                       collect (format nil "~a ~a" tier (target-key (first chain))))))
    (if parts (format nil "~{~a~^ | ~}" parts) "no chain names a model")))

(defun last-line ()
  "The last routing pass, in a line."
  (let ((decision (getf +last+ :decision))
        (analysis (getf +last+ :analysis))
        (skip (getf +last+ :skip)))
    (cond ((and analysis (null decision)) "nothing routed yet: no chain serves this tier")
          (analysis (decision-line decision analysis))
          (skip (format nil "jev skipped: ~a" skip))
          (t "nothing routed yet"))))

(defun status ()
  "Where the table is, the chains, the spend, and the last pass; no network."
  (handler-case
      (let* ((router (load-router (config-path)))
             (line (getf (spend) :line)))
        (format nil "jev: mode ~a; table ~a; ~a; ~a; last ~a"
                (setting :mode) (router-path router) (chains-line router) line (last-line)))
    (jev-error (condition) (format nil "jev: ~a" (princ-to-string condition)))))

(defun why-line ()
  "The full judgment on the last prompt, numbers and all."
  (let ((prompt (getf +last+ :prompt)))
    (unless prompt
      (return-from why-line "jev: nothing routed yet: (jev:route \"...\") or /jev route TEXT first"))
    (let ((judgment (judge prompt)))
      (format nil "jev why: kind ~a (confidence ~,2f) from ~a; probabilities ~{~a~^, ~}; complexity ~,2f/3 (~,2f); capability ~,2f/3 (~,2f); reasoning ~,2f/1; ~d ms"
              (getf judgment :kind) (getf judgment :confidence) (getf judgment :backend)
              (loop for (label . probability) in (getf judgment :probabilities)
                    collect (format nil "~a ~,2f" label probability))
              (getf judgment :complexity) (getf judgment :complexity-confidence)
              (getf judgment :capability) (getf judgment :capability-confidence)
              (getf judgment :reasoning) (getf judgment :latency-ms)))))

(defun route-line (prompt)
  "ROUTE PROMPT and say the pick in the original's one line, notes after it."
  (handler-case
      (let ((decision (route prompt)))
        (if (getf decision :skipped)
            (format nil "jev skipped: ~a" (getf decision :skipped))
            (format nil "~a ~a~@[~{; ~a~}~]"
                    (getf decision :glyph) (getf decision :line)
                    (getf decision :notes))))
    (jev-error (condition) (format nil "jev: ~a" (princ-to-string condition)))))

(defun run-slash (args session-id)
  "/jev typed alone: the table, the chains and the spend. route TEXT names
the tier a text deserves; why re-asks the last prompt; spend is the ledger."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (handler-case
        (cond ((zerop (length text)) (status))
              ((string-equal text "status") (status))
              ((string-equal text "spend") (getf (spend) :line))
              ((string-equal text "why") (why-line))
              ((uiop:string-prefix-p "route " (string-downcase text)) (route-line (subseq text 6)))
              (t (route-line text)))
      (jev-error (condition) (format nil "jev: ~a" (princ-to-string condition))))))

;;; --- the section --------------------------------------------------------------------

(nle:define-cell jev-routing
  (:section ("jev-routing")
    (:guide "set config_path to your routing JSON (routes, kindModels, budget, pricing); make a TypeSafe key at https://console.typesafe.ai/ and set TYPESAFE_API_KEY or save it in auth.json under api_keys.typesafe; base_url and model are optional (empty reads TYPESAFE_BASE_URL and TYPESAFE_DEFAULT_MODEL, else the public API and jev-latest); mode off leaves route answering (:skipped \"mode off\")")
    ("config_path" :path :doc "the routing table JSON; empty leaves the cell inert")
    ("mode" :choice :options '("shadow" "notify" "off") :default "shadow"
     :doc "shadow and notify both only advise; off makes route answer (:skipped \"mode off\")")
    ("base_url" :string :default ""
     :doc "the TypeSafe API root; empty is TYPESAFE_BASE_URL, else https://api.typesafe.ai")
    ("model" :string :default ""
     :doc "the judgment model; empty is TYPESAFE_DEFAULT_MODEL, else jev-latest")
    ("timeout_ms" :integer :default 3500 :min 100
     :doc "one judgment's ceiling in milliseconds"))
  (:help :jev "jev:route names the model tier a prompt deserves and the model your routing table holds for it" +manual+)
  (:command "jev" 'run-slash
   :description "Which model tier this prompt deserves, from your routing table"
   :argument-hint "[status|why|route TEXT|spend]"))
